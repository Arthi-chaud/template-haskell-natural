module Language.Haskell.TH.Natural.Syntax.Expr.Common where

import qualified Language.Haskell.TH as TH

data LetBinding = MkLet {_varName :: TH.Name, _bound :: TH.Exp, _strict :: Bool} deriving (Eq, Show)
data Deconstruct = MkDec {_conName :: TH.Name, _fieldVarNames :: [(Int, TH.Name)], _src :: TH.Exp, _totalFieldCount :: Int} deriving (Eq, Show)
