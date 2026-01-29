{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr (
    -- * Types
    ExprDefinition,
    -- ExprBuilder,
) where

import Control.Lens (makeLenses, use, uses, (%~), (^.), (|>))
import Control.Monad.State
import Language.Haskell.TH (Q, TExp)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder)
import Language.Haskell.TH.Natural.Syntax.Internal

type ExprDefinition a = Q (TExp a)

data ExprBuilderState (step :: k) = MkEBS
    { _argNames :: [Name]
    , _binds :: [(Name, TH.Exp)]
    , _returnedExp :: StepType step TH.Exp
    }

makeLenses ''ExprBuilderState

type instance StepType '[] a = a
type instance StepType (_ : _) a = ()

newtype ExprBuilder t prev next a = MkEB {builder :: Builder ExprBuilderState prev next a} deriving (Functor)

instance Applicative (ExprBuilder t step step) where
    pure a = MkEB $ pure a
    liftA2 pair (MkEB f1) (MkEB f2) = MkEB (liftA2 pair f1 f2)

instance Monad (ExprBuilder t step step) where
    (>>=) (MkEB (MkB f1)) f2 = MkEB $ MkB $ \s -> Prelude.do
        (s1, a) <- f1 s
        (s2, b) <- unB (builder $ f2 a) s1
        return (s2, b)

instance MonadState (ExprBuilderState step) (ExprBuilder t step step) where
    state f = MkEB $ state f

liftQ :: Q a -> ExprBuilder t step step a
liftQ q = MkEB $ lift q

type family ConsumableArg a where
    ConsumableArg (a ': b) = a

type family ConsumedArg a where
    ConsumedArg (_ ': b) = b

-- arg :: (ConsumableArg curr ~ a, ConsumedArg curr ~ b) => ExprBuilder t curr rest (TExp a)
-- arg = MkEB $ MkB $ \MkEBS{..} ->
--     let
--         prevArgCount = length _argNames
--         nextArgName = 'a' : show (prevArgCount + 1)
--      in
--         pure (MkEBS{..}{_argNames = argNames %~ (|> nextArgName)}, undefined)
--
-- returns :: (THBuilder b (TExp t'), curr ~ '[t']) => b -> ExprBuilder t curr '[] ()
-- returns = undefined
