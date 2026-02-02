{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module Language.Haskell.TH.Natural.Syntax.Internal.Builder (
    -- * Main Builders
    Builder,
    ConstBuilder,
    liftQ,

    -- * Base Builder
    BaseBuilder (..),
    runBaseBuilder,
    (>>=),
    (>>),

    -- * Steps
    BuilderStep (..),

    -- * Unsafe
    unsafeWithState,
    unsafeCastStep,
) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Language.Haskell.TH as TH
import Prelude hiding ((>>), (>>=))
import qualified Prelude

-- | A computation that builds an object, the state
--
-- A graded state monad
newtype BaseBuilder s (prev :: k) (next :: k) m a
    = MkB {unB :: StateT s m a}
    deriving (Functor)

instance (Monad m) => Applicative (BaseBuilder s step step m) where
    pure a = MkB $ pure a
    liftA2 pair (MkB f1) (MkB f2) = MkB $ do
        a <- f1
        pair a <$> f2

instance (Monad m) => Monad (BaseBuilder s step step m) where
    (>>=) (MkB f1) f2 = MkB $ Prelude.do
        a <- f1
        unB (f2 a)

instance MonadTrans (BaseBuilder s step step) where
    lift m = MkB $ lift m

instance (Monad m) => MonadReader s (BaseBuilder s step step m) where
    ask = MkB get
    local f m = MkB (modify f) Prelude.>> m

{-# INLINE runBaseBuilder #-}
runBaseBuilder :: Builder s step end () -> s -> TH.Q s
runBaseBuilder (MkB f) = execStateT f

{-# INLINE (>>=) #-}
(>>=) :: Builder s prev curr a -> (a -> Builder s curr next b) -> Builder s prev next b
(>>=) (MkB f1) f2 = MkB $ Prelude.do
    a <- f1
    unB (f2 a)

{-# INLINE (>>) #-}
(>>) :: Builder s prev curr a -> Builder s curr next b -> Builder s prev next b
(>>) f1 f2 = f1 >>= const f2

-- | Common type for anything that builds a TH AST.
type Builder s prev next a = BaseBuilder s prev next TH.Q a

-- | Similar to 'Builder', but the state is always 'Ready'
type ConstBuilder s a = BaseBuilder s () () TH.Q a

instance (Monad m) => MonadState s (BaseBuilder s step step m) where
    state f = MkB $ state f

liftQ :: TH.Q a -> BaseBuilder s step step TH.Q a
liftQ = MkB . lift

-- | Allows accessing and modifying the state.
--
-- Using this to modify the state breaks the security provided by the type-level tracking of state.
unsafeWithState :: StateT s m a -> BaseBuilder s prev curr m a
unsafeWithState = MkB

unsafeCastStep :: forall prev' curr' prev curr m s a. BaseBuilder s prev curr m a -> BaseBuilder s prev' curr' m a
unsafeCastStep (MkB m) = MkB m

data BuilderStep = Empty | Ready deriving (Eq, Show)
