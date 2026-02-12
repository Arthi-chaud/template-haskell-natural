module Language.Haskell.TH.Natural.Syntax.Expr.Simple.Typed (
    SimpleTypedExprDefinition,
    SimpleTypedExprBuilder,
    module Language.Haskell.TH.Natural.Syntax.Expr.Simple.State,
    newExpr,
    arg,
    module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Monad,
    module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class,
) where

import Language.Haskell.TH.Natural.Syntax.Expr.Simple.State
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Simple.Untyped as Untyped
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Monad
import qualified Language.Haskell.TH.Syntax as TH

type SimpleTypedExprDefinition a = TH.Q (TH.TExp a)

type SimpleTypedExprBuilder = TypedExprBuilder SimpleExprBuilderState

newExpr :: SimpleTypedExprBuilder '[] args (Returns a) () -> SimpleTypedExprDefinition (ExprType args (Returns a))
newExpr = runTypedExprBuilder

arg :: SimpleTypedExprBuilder args (AddArg args a) Unknown (TH.TExp a)
arg = unsafeUntyped $ fmap TH.TExp Untyped.arg
