{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}

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
    runBaseConstBuilder,
    (>>=),
    
    -- * Const
    zoomConst
) where

import Control.Monad.State
import Data.Functor.Const
import Data.Tuple (swap)
import qualified Language.Haskell.TH as TH
import Prelude hiding ((>>=))
import qualified Prelude
import Data.Kind (Type)

-- | A computation that builds an object, the state, whose definition is parameterised by the _step_ of the computation
--
-- Basically a graded state monad
newtype BaseBuilder s (prev :: k) next m a
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
runBaseBuilder :: Builder s step end () -> s step -> TH.Q (s end)
runBaseBuilder (MkB f) s = fmap fst (f s)

{-# INLINE runBaseConstBuilder #-}
runBaseConstBuilder :: ConstBuilder s () -> s  -> TH.Q s
runBaseConstBuilder builder s = getConst <$> runBaseBuilder builder (Const s)

{-# INLINE (>>=) #-}
(>>=) :: Builder s prev curr a -> (a -> Builder s curr next b) -> Builder s prev next b
(>>=) (MkB f1) f2 = MkB $ \s -> do
    (s1, a) <- f1 s
    (s2, b) <- unB (f2 a) s1
    return (s2, b)

-- | Common type for anything that builds a TH AST.
type Builder s (prev) (next) a = BaseBuilder s prev next TH.Q a

-- | Similar to 'Builder', but the state is always 'Ready' 
type ConstBuilder s a = BaseBuilder (Const s) Ready Ready TH.Q a

zoomConst :: Monad m => StateT s m a -> BaseBuilder (Const s) step step m a
zoomConst m = MkB $ \(Const s) -> do 
    (a, s') <-runStateT  m s
    return (Const s', a)

-- | Describes the current state of the builder
data BuilderStep
    = -- | Means that the object being built is not ready to be reified
      Empty
    | -- | The object is ready to be computed/reified
      Ready
    deriving (Eq, Ord, Show)

type StepType :: k -> Type -> Type
type family StepType (step :: k) a
type instance StepType Empty a = ()
type instance StepType Ready a = a
