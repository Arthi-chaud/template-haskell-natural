module Language.Haskell.TH.Natural.Syntax.Expr.Common (
    -- * Binding (Let, do bind)
    Binding (..),
    bindingToDec,

    -- * Deconstruction
    Deconstruct (..),
    deconstructToDec,
) where

import Control.Lens
import qualified Language.Haskell.TH as TH

data Binding = MkBind {_varName :: TH.Name, _bound :: TH.Exp, _strict :: Bool} deriving (Eq, Show)

bindingToDec :: Binding -> TH.Dec
bindingToDec (MkBind n expr s) = TH.ValD ((if s then TH.BangP else id) $ TH.VarP n) (TH.NormalB expr) []

data Deconstruct = MkDec {_conName :: TH.Name, _fieldVarNames :: [(Int, TH.Name)], _src :: TH.Exp, _totalFieldCount :: Int} deriving (Eq, Show)

deconstructToDec :: Deconstruct -> TH.Dec
deconstructToDec (MkDec cName fields src count) =
    TH.ValD (TH.ConP cName [] ([0 .. count - 1] <&> \i -> maybe TH.WildP TH.VarP (lookup i fields))) (TH.NormalB src) []
