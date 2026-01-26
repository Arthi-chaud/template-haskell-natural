module Language.Haskell.TH.Natural.Syntax.Expression where

import Language.Haskell.TH
import Language.Haskell.TH.Natural.Syntax.Internal

type ExpressionDefinition = Q Exp

newtype ExpBuilder s a = MkEB {unEB :: Builder Exp a} -- TODO
