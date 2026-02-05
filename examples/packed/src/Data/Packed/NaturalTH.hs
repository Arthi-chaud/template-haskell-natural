module Data.Packed.NaturalTH (mkPacked) where

import Data.List (singleton)
import Data.Packed.NaturalTH.Case
import Data.Packed.NaturalTH.Read
import Data.Packed.NaturalTH.Unpackable
import Language.Haskell.TH

mkPacked :: Name -> Q [Dec]
mkPacked n = do
    caseF <- genCase n
    readF <- genRead n
    unpackableI <- singleton <$> genUnpackable n
    return (caseF ++ readF ++ unpackableI)
