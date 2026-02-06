{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Natural.Syntax.Builder.Internal where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), StateT (..), execStateT, modify)
import qualified Control.Monad.State
import qualified Language.Haskell.TH as TH

-- | A computation that builds an object, the state
--
-- A graded state monad
newtype BaseBuilder m s (prev :: k) (next :: k) a
    = MkB {unB :: StateT s m a}
    deriving (Functor)

data BuilderStep = Empty | Ready deriving (Eq, Show)

-- | Common type for anything that builds a TH AST.
type Builder = BaseBuilder TH.Q

-- | Similar to 'Builder', but the state is always 'Ready'
type ConstBuilder s = BaseBuilder TH.Q s Ready Ready

-- | Runs the 'BaseBuilder' and returns the state
{-# INLINE runBaseBuilder #-}
runBaseBuilder :: (Monad m) => BaseBuilder m s step end () -> s -> m s
runBaseBuilder (MkB f) = execStateT f

-- | Similar to 'runBaseBuilder', but also returns the value output by the computation
{-# INLINE runBaseBuilder' #-}
runBaseBuilder' :: BaseBuilder m s step end a -> s -> m (a, s)
runBaseBuilder' (MkB f) = runStateT f

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

instance (MonadFail m) => MonadFail (BaseBuilder m s step step) where
    fail s = MkB $ Prelude.fail s

instance (Monad m) => MonadState s (BaseBuilder m s step step) where
    state f = MkB $ state f

-- | Lift a monadic computation in a builder
liftB :: (Monad m) => m a -> BaseBuilder m s step step a
liftB = MkB . Control.Monad.State.lift

-- | A 'safe' way to cast the current step of the 'BaseBuilder' when a computation is 'pure' and does not explicitly changes the state of the 'BaseBuilder'
impure :: BaseBuilder m s step step a -> BaseBuilder m s step next a
impure = unsafeCastStep

-- | Allows accessing and modifying the state.
--
-- Using this to modify the state breaks the security provided by the type-level tracking of state.
unsafeWithState :: StateT s m a -> BaseBuilder m s prev curr a
unsafeWithState = MkB

unsafeCastStep :: forall prev' curr' prev curr m s a. BaseBuilder m s prev curr a -> BaseBuilder m s prev' curr' a
unsafeCastStep (MkB m) = MkB m
