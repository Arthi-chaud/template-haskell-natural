{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr (
    -- * Types
    ExprDefinition,

    -- * Builder
    ExprBuilder,
    runExprBuilder,

    -- * Operations
    arg,
    returns,
    getField,
    let_,
) where

import Control.Lens (makeLenses, view, views, (.=), (?=), (^.), (|>=))
import Control.Monad
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe
import Language.Haskell.TH (Q, TExp, mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder, gen)
import Language.Haskell.TH.Natural.Syntax.Internal hiding (liftQ, (>>=))
import Language.Haskell.TH.Natural.Syntax.Internal.Builder (liftQ)
import Language.Haskell.TH.Natural.Syntax.Internal.Utils (conFieldCount)
import Language.Haskell.TH.Syntax (TExp (TExp))
import Text.Printf
import Prelude hiding ((>>=))

type ExprDefinition a = Q (TExp a)

data LetBinding = MkLet {_varName :: TH.Name, _bound :: TH.Exp} deriving (Eq, Show)

data Deconstruct = MkDec {_conName :: TH.Name, _fieldVarNames :: [(Int, TH.Name)], _src :: TH.Exp, _totalFieldCount :: Int} deriving (Eq, Show)

data ExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _lets :: [LetBinding]
    , _deconstructs :: [Deconstruct]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''ExprBuilderState

type ExprBuilder prev next a = Builder ExprBuilderState prev next a

runExprBuilder :: ExprBuilder prev Ready () -> ExprDefinition t
runExprBuilder b = do
    st <- runBaseBuilder b (MkEBS [] [] [] Nothing)
    let lamOrId = case st ^. argNames of
            [] -> id
            names -> TH.LamE (TH.VarP <$> names)
    resExp <- case st ^. returnedExp of
        Nothing -> fail "Missing returned expression"
        Just e -> return e
    let binds = st ^. lets <&> \(MkLet n expr) -> TH.ValD (TH.VarP n) (TH.NormalB expr) []
        decons = st ^. deconstructs <&> mkDeconPat
    return $ TExp $ lamOrId $ TH.LetE (binds ++ decons) resExp
  where
    mkDeconPat (MkDec cName fields src count) =
        TH.ValD (TH.ConP cName [] ([0 .. count] <&> \i -> maybe TH.WildP TH.VarP (lookup i fields))) (TH.NormalB src) []

arg :: ExprBuilder curr curr TH.Exp
arg = do
    prevArgCount <- views argNames length
    let nextArgName = mkName $ 'a' : show (prevArgCount + 1)
    argNames |>= nextArgName
    return $ TH.VarE nextArgName

returns :: (THBuilder b TH.Exp) => b -> ExprBuilder curr Ready ()
returns q = unsafeCastStep $ do
    expr <- liftQ $ gen q -- TODO Why not use just lift
    returnedExp ?= expr

let_ :: (THBuilder b TH.Exp) => b -> ExprBuilder step step TH.Exp
let_ b = do
    prevLetCount <- views lets length
    bindName <- lift $ TH.newName ("var" ++ show prevLetCount)
    expr <- lift $ gen b
    lets |>= MkLet bindName expr
    return $ TH.VarE bindName

getField :: TH.Name -> Int -> TH.Exp -> ExprBuilder step step TH.Exp
getField conName idx expr = do
    decons <- view deconstructs
    case partition ((== expr) . _src) decons of
        (_, []) -> do
            patVarName <- lift $ TH.newName "pat"
            fieldCount <- lift $ conFieldCount conName
            let newDecons = MkDec conName [(idx, patVarName)] expr fieldCount
            deconstructs |>= newDecons
            return $ TH.VarE patVarName
        (decons', prevDecons : decons'') -> do
            when (_conName prevDecons /= conName) $
                fail $
                    printf "The following expression has already been deconstructed with the %s constructor: %s" (show $ _conName prevDecons) (show expr)
            when (isJust $ lookup idx (_fieldVarNames prevDecons)) $
                fail $
                    printf "When deconstructing the following expression, the field at index %d in constructor %s has already been bound: %s" idx (show conName) (show expr)
            patVarName <- lift $ TH.newName "pat"
            let newDecons = MkDec conName ((idx, patVarName) : _fieldVarNames prevDecons) expr (_totalFieldCount prevDecons)
            deconstructs .= (decons' ++ [newDecons] ++ decons'')
            return $ TH.VarE patVarName

-- TODO do notation
-- TODO Avoid having to do type application on gen
