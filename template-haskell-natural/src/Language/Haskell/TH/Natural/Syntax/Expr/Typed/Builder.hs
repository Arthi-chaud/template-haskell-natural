{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder (
    TypedExprBuilder (..),
    unsafeUntyped,
    Returns (..),

    -- * Util type families
    (:>),
    (:++:),
    (:~>),
) where

import Data.Kind as K
import Language.Haskell.TH.Natural.Syntax.Builder

data Returns a = Unknown | Returns a

-- | Adds an argument at the end of an args list (snoc)
type family args :> arg where
    '[] :> a = '[a]
    (arg ': args) :> a = arg ': (args :> a)

-- | Merges two args lists
type family args0 :++: args1 where
    '[] :++: args1 = args1
    (arg ': args0) :++: args1 = arg ': args0 :++: args1

-- | Builds the final arrow type from a list of args and a 'Returns'
type family args :~> res where
    '[] :~> (Returns a) = a
    (arg ': args) :~> (Returns a) = arg -> (args :~> Returns a)

newtype TypedExprBuilder s (prevArgs :: [K.Type]) (args :: [K.Type]) (res :: k) a = MkTEB {unTEB :: ConstBuilder s a}

unsafeUntyped :: Builder s step step1 a -> TypedExprBuilder s args0 args res a
unsafeUntyped = MkTEB . unsafeCastStep
