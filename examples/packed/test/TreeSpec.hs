{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeSpec (spec) where

import Data.Packed hiding (mkPacked)
import Data.Packed.NaturalTH (mkPacked)
import Test.Hspec

data Tree a = Leaf a | Node (Tree a) (Tree a)

mkPacked ''Tree

spec :: Spec
spec = return ()
