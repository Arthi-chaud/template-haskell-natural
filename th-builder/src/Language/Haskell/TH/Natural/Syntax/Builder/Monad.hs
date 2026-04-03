{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Natural.Syntax.Builder.Monad where

import Language.Haskell.TH.Natural.Syntax.Builder.Internal
import Prelude hiding (fail, (>>), (>>=))
import qualified Prelude

-- | Binding operator for 'BaseBuilder'
{-# INLINE (>>=) #-}
(>>=) :: (Monad m) => BaseBuilder m s prev curr a -> (a -> BaseBuilder m s curr next b) -> BaseBuilder m s prev next b
(>>=) (MkB f1) f2 = MkB $ Prelude.do
    a <- f1
    unB (f2 a)

-- | Sequence operator for 'BaseBuilder'
{-# INLINE (>>) #-}
(>>) :: (Monad m) => BaseBuilder m s prev curr a -> BaseBuilder m s curr next b -> BaseBuilder m s prev next b
(>>) f1 f2 = f1 >>= const f2

-- | Invokes the 'Prelude.fail' function of the underlying monad (m)
fail :: (MonadFail m) => String -> BaseBuilder m s prev curr a
fail s = MkB $ Prelude.fail s
