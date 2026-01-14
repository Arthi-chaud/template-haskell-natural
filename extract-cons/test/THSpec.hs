{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module THSpec (spec) where

import Data.Constructor.Extract
import Test.Hspec

extractConstructor 'Nothing
extractConstructor 'Just

-- TODO Remove me
deriving instance Show Nothing
deriving instance Eq Nothing

deriving instance (Show a) => Show (Just a)
deriving instance (Eq a) => Eq (Just a)

spec :: Spec
spec =
    describe "Convert Constructors" $ do
        describe "'from'ExtractedCon" $ do
            it "Convert MkNothing" $
                fromExtractedCon MkNothing `shouldBe` (Nothing @(Maybe Int))
            it "Convert MkJust" $
                fromExtractedCon (MkJust 'a') `shouldBe` Just 'a'

        describe "'to' ExtractedCon" $ do
            it "Convert Nothing" $
                toExtractedCon (Nothing @(Maybe Int)) `shouldBe` Just MkNothing
            it "Convert Just" $
                toExtractedCon (Just 'a') `shouldBe` Just (MkJust 'a')
