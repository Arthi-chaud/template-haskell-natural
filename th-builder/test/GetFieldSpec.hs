{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module GetFieldSpec (spec) where

import GetField
import Language.Haskell.TH.Gen
import Test.Hspec

genDecs mkFst
genDecs mkGetSumLeft

spec :: Spec
spec = do
    describe "Get Field" $ do
        it "from pattern synonym" $ fstPair ('a', 'b') `shouldBe` 'a'
        it "from infix constructor" $ getSumLeft (1 :+: 2) `shouldBe` 1
