module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder where

import Data.Kind (Type)
import Language.Haskell.TH.Natural.Syntax.Builder

data Returns a = Unknown | Returns a

newtype TypedExprBuilder s (args :: [Type]) (res :: k) a = MkTEB {unTEB :: ConstBuilder s a}

unsafeUntyped :: Builder s step step1 a -> TypedExprBuilder s args res a
unsafeUntyped = MkTEB . unsafeCastStep
