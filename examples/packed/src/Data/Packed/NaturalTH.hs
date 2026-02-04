module Data.Packed.NaturalTH (mkPacked) where

import Data.Packed.NaturalTH.Case (genCase)
import Language.Haskell.TH

mkPacked :: Name -> Q [Dec]
mkPacked n = do
    caseF <- genCase n
    return caseF
