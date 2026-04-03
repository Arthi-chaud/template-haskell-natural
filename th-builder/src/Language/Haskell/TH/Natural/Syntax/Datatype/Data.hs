{-# LANGUAGE QualifiedDo #-}

-- | 'Builder' for data type declaration
module Language.Haskell.TH.Natural.Syntax.Datatype.Data (
    -- * Type
    newData,
    DataBuilder,

    -- * Functions
    addCon,
    setKind,
    addDeriving,
    addDeriving',

    -- ** Reexports
    addContext,
    addTypeVar,
    addTypeVar',
) where

import Control.Lens
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Common
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Syntax.ExtractedCons

type DataBuilder = ConstBuilder DataD

-- | Builds a new data type declaration. The first argument is the name of the data type.
newData :: String -> DataBuilder () -> TH.Q DataD
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

-- | Add a constructor to the data type
addCon :: (GenCon b) => b -> DataBuilder ()
addCon b = B.do
    newCon <- liftB $ genCon b
    con |>= newCon
