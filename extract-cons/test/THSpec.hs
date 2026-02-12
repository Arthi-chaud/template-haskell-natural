{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module THSpec (spec) where

import Data.Constructor.Extract
import Test.Hspec

extractConstructorsOf ''Maybe (defaultOptions{deriveClasses = [''Eq, ''Show]})

spec :: Spec
spec =
    describe "Convert Constructors" $ do
        describe "'from'ExtractedCon" $ do
            it "Convert MkNothing" $
                fromEC MkNothing `shouldBe` (Nothing @(Maybe Int))
            it "Convert MkJust" $
                fromEC (MkJust 'a') `shouldBe` Just 'a'

        describe "'to' ExtractedCon" $ do
            it "Convert Nothing" $
                toEC (Nothing @(Maybe Int)) `shouldBe` Just MkNothing
            it "Convert Just" $
                toEC (Just 'a') `shouldBe` Just (MkJust 'a')
