{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TupleSpec (spec) where

import Test.Hspec
import Tuple

generateTupleBoilerplate 4

spec :: Spec
spec = it "should get field" $ do
    let obj = (1 :: Int, 'b', ['c'], ())
    _2 obj `shouldBe` 'b'
    _4 obj `shouldBe` ()
