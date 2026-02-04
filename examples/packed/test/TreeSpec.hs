{-# LANGUAGE DataKinds #-}

module TreeSpec (Spec) where

import Data.Packed.NaturalTH (mkPacked)
import Test.Hspec

data Tree a = Leaf a | Node (Tree a) (Tree a)

mkPacked ''Tree

spec :: Spec
spec = return ()
