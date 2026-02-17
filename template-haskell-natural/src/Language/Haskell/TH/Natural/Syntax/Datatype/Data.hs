{-# LANGUAGE QualifiedDo #-}

module Language.Haskell.TH.Natural.Syntax.Datatype.Data (
    -- * Type
    DataDefinition,
    DataBuilder,
    newData,

    -- * Functions
    addCon,
    addContext,
    addTypeVar,
    addDeriving,
    setKind,
    addTypeVar',
    addDeriving',
) where

import Control.Lens
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Common
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Syntax.ExtractedCons

type DataDefinition = TH.Q DataD

type DataBuilder = ConstBuilder DataD

newData :: String -> DataBuilder () -> DataDefinition
newData dataNameStr builder = runBaseBuilder builder baseData
  where
    baseData = MkDataD [] dataName [] Nothing [] []
    dataName = TH.mkName dataNameStr

setKind :: TH.Type -> DataBuilder ()
setKind k = kind ?= k

-- | Add a deriving clause using the 'Name' of the typeclass to derive
--
-- @
--   addDeriving 'Show
-- @
addDeriving :: TH.Name -> DataBuilder ()
addDeriving tyN = addDeriving' $ TH.DerivClause Nothing [TH.ConT tyN]

addDeriving' :: TH.DerivClause -> DataBuilder ()
addDeriving' dc = derive |>= dc

addCon :: (GenCon b) => b -> DataBuilder ()
addCon b = B.do
    newCon <- liftB $ genCon b
    con |>= newCon
