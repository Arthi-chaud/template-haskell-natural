{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CurrySpec (spec) where

import Curry
import Test.Hspec

genCurries 3

spec :: Spec
spec = it "should curry" $ do
    let res = curry3 (\(_, b, _) -> snd b) 'a' ('b', 'c') 'd'
    res `shouldBe` 'c'
