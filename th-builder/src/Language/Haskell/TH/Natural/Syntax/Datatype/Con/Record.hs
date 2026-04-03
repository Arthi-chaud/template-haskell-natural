-- | 'Builder' for record constructors
module Language.Haskell.TH.Natural.Syntax.Datatype.Con.Record (
    -- * Type
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

type RecordConBuilder = ConstBuilder RecC

-- | Builds a record constructor
newRecordCon :: String -> RecordConBuilder () -> TH.Q RecC
newRecordCon conN b = runBaseBuilder b (MkRecC (TH.mkName conN) [])

-- | Add a field to the constructor
addField :: String -> TH.Type -> RecordConBuilder ()
addField n t = addField' (n, defaultBang, t)

-- | Same as 'addField', but allow setting the field's 'Kind'
addField' :: (String, TH.Bang, TH.Type) -> RecordConBuilder ()
addField' (n, b, t) = vbts |>= (TH.mkName n, b, t)
