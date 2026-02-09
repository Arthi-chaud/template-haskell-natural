module Language.Haskell.TH.Natural.Syntax.Expr.Internal (
    -- * Binding (Let, do bind)
    Binding (..),
    bindingToDec,

    -- * Deconstruction
    Deconstruct (..),
    deconstructToDec,
) where

import Control.Lens
import Data.Maybe
import qualified Language.Haskell.TH as TH

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

deconstructToDec :: Deconstruct -> TH.Dec
deconstructToDec (MkDec con fields src count) =
    let conPat = either (const TH.TupP) (`TH.ConP` []) con
     in TH.ValD (conPat ([0 .. count - 1] <&> \i -> fromMaybe TH.WildP (lookup i fields))) (TH.NormalB src) []
