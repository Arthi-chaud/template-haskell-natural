module Language.Haskell.TH.Natural.Syntax.Datatype.Con.Record (
    -- * Type
    RecordConDefinition,
    RecordConBuilder,
    newRecordCon,

    -- * Functions
    addField,
    addField',
) where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Datatype.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type RecordConDefinition = TH.Q RecC

type RecordConBuilder = ConstBuilder RecC

newRecordCon :: String -> RecordConBuilder () -> RecordConDefinition
newRecordCon conN b = runBaseBuilder b (MkRecC (TH.mkName conN) [])

addField :: String -> TH.Type -> RecordConBuilder ()
addField n t = addField' (n, defaultBang, t)

addField' :: (String, TH.Bang, TH.Type) -> RecordConBuilder ()
addField' (n, b, t) = vbts |>= (TH.mkName n, b, t)
