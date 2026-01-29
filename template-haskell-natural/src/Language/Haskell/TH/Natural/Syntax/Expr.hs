{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr (
    -- * Types
    ExprDefinition,
    -- ExprBuilder,
) where

import Control.Lens (makeLenses)
import Language.Haskell.TH (Q, TExp)
import Language.Haskell.TH.Natural.Syntax.Internal

type ExprDefinition a = Q (TExp a)

data ExprBuilderState (step :: k) = MkEBS
    { _argNames :: [Name]
    }

newtype ExprBuilder t prev next a = MkEB {builder :: BaseBuilder ExprBuilderState prev next Q a}

makeLenses ''ExprBuilderState

-- type
--
--
type family ConsumableArg a where
    ConsumableArg (a ': b) = a

type family ConsumedArg a where
    ConsumedArg (_ ': b) = b

arg :: (ConsumableArg curr ~ a, ConsumedArg curr ~ b) => ExprBuilder t curr rest (TExp a)
arg = undefined
