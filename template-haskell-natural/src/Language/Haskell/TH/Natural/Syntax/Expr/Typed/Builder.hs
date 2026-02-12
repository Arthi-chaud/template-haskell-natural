{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder (
    TypedExprBuilder (..),
    unsafeUntyped,
    Returns (..),

    -- * Util type families
    AddArg,
    (:++:),
    ExprType,
) where

import Data.Kind as K
import Language.Haskell.TH.Natural.Syntax.Builder

data Returns a = Unknown | Returns a

type family AddArg args a where
    AddArg '[] a = '[a]
    AddArg (arg ': args) a = arg ': AddArg args a

type family args0 :++: args1 where
    '[] :++: args1 = args1
    (arg ': args0) :++: args1 = arg ': args0 :++: args1

type family ExprType args res where
    ExprType '[] (Returns a) = a
    ExprType (arg ': args) (Returns a) = arg -> ExprType args (Returns a)

newtype TypedExprBuilder s (prevArgs :: [K.Type]) (args :: [K.Type]) (res :: k) a = MkTEB {unTEB :: ConstBuilder s a}

unsafeUntyped :: Builder s step step1 a -> TypedExprBuilder s args0 args res a
unsafeUntyped = MkTEB . unsafeCastStep
