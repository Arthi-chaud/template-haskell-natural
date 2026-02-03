{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.TH.Natural.Syntax.Internal.Builder (
    -- * Main Builders
    Builder,
    ConstBuilder,
    liftB,

    -- * Base Builder
    BaseBuilder (..),
    runBaseBuilder,
    (>>=),
    (>>),

    -- * Steps
    BuilderStep (..),

    -- * Utils
    impure,

    -- * Unsafe
    unsafeWithState,
    unsafeCastStep,
) where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), StateT (..), execStateT, modify)
import qualified Control.Monad.State
import qualified Language.Haskell.TH as TH
import Prelude hiding ((>>), (>>=))
import qualified Prelude

-- | A computation that builds an object, the state
--
-- A graded state monad
newtype BaseBuilder m s (prev :: k) (next :: k) a
    = MkB {unB :: StateT s m a}
    deriving (Functor)

instance (Monad m) => Applicative (BaseBuilder m s step step) where
    pure a = MkB $ pure a
    liftA2 pair (MkB f1) (MkB f2) = MkB $ do
        a <- f1
        pair a <$> f2

instance (Monad m) => Monad (BaseBuilder m s step step) where
    (>>=) (MkB f1) f2 = MkB $ Prelude.do
        a <- f1
        unB (f2 a)

instance (Monad m) => MonadReader s (BaseBuilder m s step step) where
    ask = MkB get
    local f m = MkB (modify f) Prelude.>> m

{-# INLINE runBaseBuilder #-}
runBaseBuilder :: (Monad m) => BaseBuilder m s step end () -> s -> m s
runBaseBuilder (MkB f) = execStateT f

{-# INLINE (>>=) #-}
(>>=) :: (Monad m) => BaseBuilder m s prev curr a -> (a -> BaseBuilder m s curr next b) -> BaseBuilder m s prev next b
(>>=) (MkB f1) f2 = MkB $ Prelude.do
    a <- f1
    unB (f2 a)

{-# INLINE (>>) #-}
(>>) :: (Monad m) => BaseBuilder m s prev curr a -> BaseBuilder m s curr next b -> BaseBuilder m s prev next b
(>>) f1 f2 = f1 >>= const f2

-- | Common type for anything that builds a TH AST.
type Builder = BaseBuilder TH.Q

-- | Similar to 'Builder', but the state is always 'Ready'
type ConstBuilder s = BaseBuilder TH.Q s () ()

instance (MonadFail m) => MonadFail (BaseBuilder m s step step) where
    fail s = MkB $ fail s

instance (Monad m) => MonadState s (BaseBuilder m s step step) where
    state f = MkB $ state f

liftB :: (Monad m) => m a -> BaseBuilder m s step step a
liftB = MkB . Control.Monad.State.lift

-- | Allows accessing and modifying the state.
--
-- Using this to modify the state breaks the security provided by the type-level tracking of state.
unsafeWithState :: StateT s m a -> BaseBuilder m s prev curr a
unsafeWithState = MkB

unsafeCastStep :: forall prev' curr' prev curr m s a. BaseBuilder m s prev curr a -> BaseBuilder m s prev' curr' a
unsafeCastStep (MkB m) = MkB m

impure :: BaseBuilder m s step step a -> BaseBuilder m s step next a
impure = unsafeCastStep

data BuilderStep = Empty | Ready deriving (Eq, Show)
