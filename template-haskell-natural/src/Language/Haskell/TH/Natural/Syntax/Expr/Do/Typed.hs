{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Do.Typed (
    DoTypedExprDefinition,
    DoTypedExprBuilder,
    newDo,
    stmt,
    strictBind,
    bind,
    bind_,
    module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Monad,
    module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class,
    module Language.Haskell.TH.Natural.Syntax.Expr.Do.State,
) where

import Language.Haskell.TH
import Language.Haskell.TH.Gen (GenTExpr (genTExpr))
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Do.State
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Do.Untyped as Untyped
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class as Typed
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Monad
import qualified Language.Haskell.TH.Syntax as TH

type DoTypedExprDefinition a = TH.Q (TH.TExp a)

type DoTypedExprBuilder = TypedExprBuilder DoExprBuilderState '[] '[]

newDo :: DoTypedExprBuilder (Returns a) () -> DoTypedExprDefinition a
newDo = runTypedExprBuilder

stmt :: (GenTExpr t b) => b -> DoTypedExprBuilder (Returns t) ()
stmt = Typed.returns

strictBind :: (GenTExpr t b) => b -> DoTypedExprBuilder Unknown (TH.TExp t)
strictBind = bind_ True

bind :: (GenTExpr t b) => b -> DoTypedExprBuilder Unknown (TH.TExp t)
bind = bind_ False

bind_ :: forall t b. (GenTExpr t b) => Bool -> b -> DoTypedExprBuilder Unknown (TH.TExp t)
bind_ s b = unsafeUntyped $ Untyped.do
    e <- unType <$> liftB (genTExpr @t b)
    TH.TExp <$> Untyped.bind_ s e
