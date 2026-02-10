{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Copied from github.com/granule-project/deriving-distributed-linear-haskell/blob/main/src/Linear/Box.hs
module Linear.Box (Box (..), Pushable (..)) where

import Control.Monad
import Data.Foldable
import Data.Traversable
import Prelude.Linear as Linear

data Box r a where
    Box :: a %r -> Box r a
    deriving (Show, Functor, Foldable, Traversable)

class Pushable f where
    push :: Box r (f a) %1 -> f (Box r a)
