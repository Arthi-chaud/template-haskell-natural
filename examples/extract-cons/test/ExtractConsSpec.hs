module ExtractConsSpec (spec) where

import Data.Constructor.Extract (defaultOptions)
import Data.Constructor.Extract.Class
import Data.Constructor.Extract.Natural (extractConstructor)
import Language.Haskell.TH
import Test.Hspec

extractConstructor 'VarT defaultOptions

spec :: Spec
spec = pure ()
