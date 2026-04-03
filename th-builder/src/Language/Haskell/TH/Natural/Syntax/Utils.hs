-- | QoL / Utility functions
module Language.Haskell.TH.Natural.Syntax.Utils (apply, applyT) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen

-- | Applies the given list of arguments to the expression
apply :: (GenExpr f, GenExpr arg) => f -> [arg] -> TH.Q TH.Exp
apply f = foldl (\rest arg -> [|$rest $(genExpr arg)|]) (genExpr f)

-- | Applies the given list of type arguments to the type
applyT :: (GenType f, GenType arg) => f -> [arg] -> TH.Q TH.Type
applyT f = foldl (\rest arg -> [t|$rest $(genTy arg)|]) (genTy f)
