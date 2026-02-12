module Language.Haskell.TH.Natural.Syntax.Expr.Untyped.Base (apply) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.QBuilder

apply :: (QBuilder f TH.Exp, QBuilder arg TH.Exp) => f -> [arg] -> TH.Q TH.Exp
apply f = foldl (\rest arg -> [|$(gen rest) $(gen arg)|]) (gen f)
