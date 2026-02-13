{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module PatSynSpec (spec) where

import Language.Haskell.TH.Gen
import PatSyn
import Test.Hspec

genDecs mkFst

spec :: Spec
spec = it "should get fst" $ fstPair ('a', 'b') `shouldBe` 'a'
