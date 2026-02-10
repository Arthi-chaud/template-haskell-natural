{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Simple.Typed where

import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Language.Haskell.TH.QBuilder
import qualified Language.Haskell.TH.Syntax as TH

type SimpleTypedExprDefinition a = TH.Q (TH.TExp a)

type SimpleTypedExprBuilder = TypedExprBuilder SimpleExprBuilderState

newExpr :: SimpleTypedExprBuilder args (Returns a) () -> SimpleExprDefinition
newExpr = undefined

arg :: SimpleTypedExprBuilder (a ': args) Unknown (TH.TExp a)
arg = undefined

returns :: (QBuilder b (TH.TExp a)) => b -> SimpleTypedExprBuilder args (Returns a) ()
returns = undefined

-- TODO Instance of typed typeclass
