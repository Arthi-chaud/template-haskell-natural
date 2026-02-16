module TupleSpec (spec) where

import Test.Hspec
import TupleTH.Natural

spec :: Spec
spec = do
    it "map" $
        $(mapTuple 2) (+ 1) (1, 2 :: Int) `shouldBe` (2, 3)
    it "from list" $
        $(safeTupleFromList 3) ['a', 'b', 'c'] `shouldBe` Just ('a', 'b', 'c')
    it "filter" $
        $(filterTuple 3) (== 'a') ('a', 'b', 'c') `shouldBe` ['a']
    it "reverse" $
        $(reverseTuple 3) ('a', 'b', 'c') `shouldBe` ('c', 'b', 'a')
    it "delete at" $
        $(deleteAtTuple 3 2) ('a', 'b', 'c') `shouldBe` ('a', 'b')
    it "update at" $
        $(updateAtTuple 3 1) (const (1 :: Int)) ('b', 'c', 'a') `shouldBe` ('b', 1, 'a')
    it "elem" $
        $(elemTuple 3) 'a' ('b', 'c', 'a') `shouldBe` True
    it "proj" $
        $(proj 3 1) ('b', 'c', 'a') `shouldBe` 'c'
    describe "Fold" $ do
        it "or" $
            $(orTuple 2) (True, False) `shouldBe` True
        it "and" $
            $(andTuple 2) (True, False) `shouldBe` False
        it "sum" $
            $(sumTuple 2) (3, 5 :: Int) `shouldBe` 8
        it "any" $
            $(anyTuple 2) even (3, 4 :: Int) `shouldBe` True
    it "cat" $
        $(catTuples 3 3) (1, 2, 3) ('b', 'c', 'a') `shouldBe` (1, 2, 3, 'b', 'c', 'a')
    it "zip" $
        $(zipTuple 3 3) (1, 2, 3) ('b', 'c', 'a') `shouldBe` [(1, 'b'), (2, 'c'), (3, 'a')]
