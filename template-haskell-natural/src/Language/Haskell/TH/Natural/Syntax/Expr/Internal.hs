{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Internal (
    -- * Binding (Let, do bind)
    Binding (..),
    bindingToDec,

    -- * Deconstruction
    Deconstruct (..),
    deconstructToDec,
    deconstructToPat,
    mergeDeconstructs,
    mergeDeconstruct,
    --- * Lenses
    conName,
    fieldPatterns,
    src,
    totalFieldCount,
) where

import Control.Lens
import Control.Monad
import Data.List (intersectBy, partition)
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Text.Printf

data Binding = MkBind {_varName :: TH.Name, _bound :: TH.Exp, _strict :: Bool} deriving (Eq, Show)

bindingToDec :: Binding -> TH.Dec
bindingToDec (MkBind n expr s) = TH.ValD ((if s then TH.BangP else id) $ TH.VarP n) (TH.NormalB expr) []

data Deconstruct = MkDec
    { _conName :: Either Int TH.Name
    -- ^ Left is for tuples. The Int represents the size of the tuple
    , _fieldPatterns :: [(Int, TH.Pat)]
    , _src :: TH.Exp
    , _totalFieldCount :: Int
    }
    deriving (Eq, Show)

makeLenses ''Deconstruct

deconstructToPat :: Deconstruct -> TH.Pat
deconstructToPat MkDec{..} =
    let conPat = either (const TH.TupP) (`TH.ConP` []) _conName
     in conPat ([0 .. _totalFieldCount - 1] <&> \i -> fromMaybe TH.WildP (lookup i _fieldPatterns))

deconstructToDec :: Deconstruct -> TH.Dec
deconstructToDec d = TH.ValD (deconstructToPat d) (TH.NormalB $ _src d) []

-- | Merge 'Deconstruct's that have the same 'src'
mergeDeconstructs :: (MonadFail m) => [Deconstruct] -> m [Deconstruct]
mergeDeconstructs [] = pure []
mergeDeconstructs (d : ds) = case partition (\d' -> d ^. src == d' ^. src) ds of
    ([], _) -> (d :) <$> mergeDeconstructs ds
    (relatedDs, ds') -> do
        d' <- foldM mergeDeconstruct d relatedDs
        (d' :) <$> mergeDeconstructs ds'

-- | Merge two 'Deconstruct's. Will fail if the constructor does not match or if the totalFeildCount are not equal
mergeDeconstruct :: (MonadFail m) => Deconstruct -> Deconstruct -> m Deconstruct
mergeDeconstruct d1 d2 = do
    when (d1 ^. conName /= d2 ^. conName) $
        Prelude.fail $
            printf
                "The following expression has already been deconstructed with the %s constructor: %s"
                (show $ d1 ^. conName)
                (show $ d1 ^. src)
    let duplicates = intersectBy (\(a, _) (b, _) -> a == b) (d1 ^. fieldPatterns) (d2 ^. fieldPatterns)
    unless (null duplicates) $
        Prelude.fail $
            printf
                "When deconstructing the following expression,  fields %s have been bounds twice or more: %s"
                (show duplicates)
                (show $ d1 ^. src)
    pure $ d1{_fieldPatterns = d1 ^. fieldPatterns ++ d2 ^. fieldPatterns}
