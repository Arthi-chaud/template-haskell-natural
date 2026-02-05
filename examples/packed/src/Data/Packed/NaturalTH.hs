module Data.Packed.NaturalTH (mkPacked) where

import Control.Monad (forM)
import Data.List (singleton)
import Data.Packed.NaturalTH.Case
import Data.Packed.NaturalTH.Packable
import Data.Packed.NaturalTH.Read
import Data.Packed.NaturalTH.Unpackable
import Data.Packed.NaturalTH.Write (genConWrite, genWrite)
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH

mkPacked :: Name -> Q [Dec]
mkPacked n = do
    (TH.TyConI (TH.DataD _ _ _ _ cs _)) <- TH.reify n
    caseF <- genCase n
    readF <- genRead n
    conWriteF <- concat <$> forM (cs `zip` [0 ..]) (uncurry (genConWrite n))
    writeF <- genWrite n
    unpackableI <- singleton <$> genUnpackable n
    packableI <- singleton <$> genPackable n
    return (caseF ++ readF ++ unpackableI ++ packableI ++ conWriteF ++ writeF)
