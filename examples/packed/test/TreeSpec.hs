{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TreeSpec (spec) where

import Data.Packed hiding (mkPacked)
import Data.Packed.NaturalTH (mkPacked)
import Test.Hspec

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

mkPacked ''Tree

spec :: Spec
spec = do
    let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3)) :: Tree Int
    it "should pack/unpack tree" $
        let packedTree = pack tree
            (tree1, _) = unpack packedTree
         in tree1 `shouldBe` tree
