{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Monad where

import qualified Language.Haskell.TH.Natural.Syntax.Builder.Monad as B
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Prelude hiding (fail, (>>), (>>=))

{-# INLINE (>>=) #-}
(>>=) ::
    TypedExprBuilder s args0 args0' res0 a ->
    (a -> TypedExprBuilder s '[] args1' res1 b) ->
    TypedExprBuilder s args0 (args0' :++: args1') res1 b
(>>=) (MkTEB f1) f2 = MkTEB $ B.do
    a <- f1
    unTEB (f2 a)

{-# INLINE (>>) #-}
(>>) ::
    TypedExprBuilder s args0 args0' res0 a ->
    TypedExprBuilder s '[] args1' res1 b ->
    TypedExprBuilder s args0 (args0' :++: args1') res1 b
(>>) (MkTEB f1) (MkTEB f2) = MkTEB $ f1 B.>> f2

fail :: String -> TypedExprBuilder s args args res a
fail = MkTEB . B.fail
