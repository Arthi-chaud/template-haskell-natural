module Language.Haskell.TH.Natural.Syntax.Expr.Untyped.Base (apply) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen

apply :: (GenExpr f, GenExpr arg) => f -> [arg] -> TH.Q TH.Exp
apply f = foldl (\rest arg -> [|$rest $(genExpr arg)|]) (genExpr f)
