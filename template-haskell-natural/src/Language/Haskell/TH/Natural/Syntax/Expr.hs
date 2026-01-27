module Language.Haskell.TH.Natural.Syntax.Expr (
    -- * Types
    ExprDefinition,
    ExprBuilder,
) where

import Data.Functor.Const
import Language.Haskell.TH
import Language.Haskell.TH.Natural.Syntax.Internal

type ExprDefinition = Q Exp

newtype ExprBuilder s a = MkEB {unEB :: Builder (Const Exp) Ready Ready a} -- TODO
