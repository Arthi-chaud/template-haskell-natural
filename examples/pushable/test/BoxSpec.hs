{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}

module BoxSpec (spec) where

import Linear.Box
import Linear.TH
import Test.Hspec

data List a where
    Cons :: a %1 -> List a %1 -> List a
    Nil :: List a

derivePushable ''List

spec :: Spec
spec = pure ()
