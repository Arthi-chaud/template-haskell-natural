{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.TH.Natural.Syntax.Internal.Builder (
    -- * Main Builders
    Builder,
    ConstBuilder,

    -- * Steps
    BuilderStep (..),
    StepType,

    -- * Base Builder
    BaseBuilder (..),
    runBaseBuilder,
    (>>=),

    -- * Const Builder
    BaseConstBuilder (..),
    runBaseConstBuilder,
) where

import Control.Monad.State
import Data.Functor.Const
import Data.Tuple (swap)
import qualified Language.Haskell.TH as TH
import Prelude hiding ((>>=))
import qualified Prelude

-- | A computation that builds an object, the state, whose definition is parameterised by the _step_ of the computation
--
-- Basically a graded state monad
newtype BaseBuilder s (prev :: BuilderStep) (next :: BuilderStep) m a
    = MkB {unB :: s prev -> m (s next, a)}
    deriving (Functor)

instance (Monad m) => Applicative (BaseBuilder s step step m) where
    pure a = MkB (\s -> pure (s, a))
    liftA2 pair (MkB f1) (MkB f2) = MkB $ \s -> do
        (s1, a) <- f1 s
        (s2, b) <- f2 s1
        return (s2, pair a b)

instance (Monad m) => Monad (BaseBuilder s step step m) where
    (>>=) (MkB f1) f2 = MkB $ \s -> Prelude.do
        (s1, a) <- f1 s
        (s2, b) <- unB (f2 a) s1
        return (s2, b)

instance (Monad m) => MonadState (s step) (BaseBuilder s step step m) where
    state f = MkB $ \s -> pure $ swap $ f s

instance MonadTrans (BaseBuilder s step step) where
    lift m = MkB $ \s -> (s,) <$> m

{-# INLINE runBaseBuilder #-}
runBaseBuilder :: Builder s step Ready () -> s step -> TH.Q (s Ready)
runBaseBuilder (MkB f) s = fmap fst (f s)

{-# INLINE (>>=) #-}
(>>=) :: Builder s prev curr a -> (a -> Builder s curr next b) -> Builder s prev next b
(>>=) (MkB f1) f2 = MkB $ \s -> do
    (s1, a) <- f1 s
    (s2, b) <- unB (f2 a) s1
    return (s2, b)

-- | Common type for anything that builds a TH AST.
type Builder s (prev :: BuilderStep) (next :: BuilderStep) a = BaseBuilder s prev next TH.Q a

-- | A less general 'BaseBuilder' where the state is not parameterised by the step
newtype BaseConstBuilder s m a = MkCB {unCB :: BaseBuilder (Const s) Ready Ready m a} deriving (Functor)

instance (Monad m) => Applicative (BaseConstBuilder s m) where
    pure a = MkCB $ pure a
    liftA2 pair (MkCB f1) (MkCB f2) = MkCB $ liftA2 pair f1 f2

instance (Monad m) => Monad (BaseConstBuilder s m) where
    (>>=) (MkCB f1) f2 = MkCB $ f1 Prelude.>>= (unCB . f2)

instance (Monad m) => MonadState s (BaseConstBuilder s m) where
    state f = MkCB $ MkB $ \(Const s) -> let (a, s') = f s in pure (Const s', a)

instance MonadTrans (BaseConstBuilder s) where
    lift m = MkCB $ lift m

{-# INLINE runBaseConstBuilder #-}
runBaseConstBuilder :: ConstBuilder s () -> s -> TH.Q s
runBaseConstBuilder (MkCB f) s = getConst <$> runBaseBuilder f (Const s)

-- | Common type for anything that builds a TH AST.
type ConstBuilder s a = BaseConstBuilder s TH.Q a

-- | Describes the current state of the builder
data BuilderStep
    = -- | Means that the object being built is not ready to be reified
      Empty
    | -- | The object is ready to be computed/reified
      Ready
    deriving (Eq, Ord, Show)

type family StepType step a where
    StepType Empty a = ()
    StepType Ready a = a
