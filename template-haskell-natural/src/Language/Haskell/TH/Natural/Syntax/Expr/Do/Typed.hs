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
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Do.State
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Do.Untyped as Untyped
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class as Typed
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Monad
import Language.Haskell.TH.QBuilder
import qualified Language.Haskell.TH.Syntax as TH

type DoTypedExprDefinition a = TH.Q (TH.TExp a)

type DoTypedExprBuilder = TypedExprBuilder DoExprBuilderState '[] '[]

newDo :: DoTypedExprBuilder (Returns a) () -> DoTypedExprDefinition a
newDo = runTypedExprBuilder

stmt :: (QBuilder b (TH.TExp a)) => b -> DoTypedExprBuilder (Returns a) ()
stmt = Typed.returns

strictBind :: (QBuilder b (TH.TExp a)) => b -> DoTypedExprBuilder Unknown (TH.TExp a)
strictBind = bind_ True

bind :: (QBuilder b (TH.TExp a)) => b -> DoTypedExprBuilder Unknown (TH.TExp a)
bind = bind_ False

bind_ :: forall a b. (QBuilder b (TH.TExp a)) => Bool -> b -> DoTypedExprBuilder Unknown (TH.TExp a)
bind_ s b = unsafeUntyped $ Untyped.do
    e <- unType <$> liftB (gen @b @(TH.TExp a) b)
    TH.TExp <$> Untyped.bind_ s e
