-- | 'Builder' for GADT constructor
module Language.Haskell.TH.Natural.Syntax.Datatype.Con.GADT (
    -- * Type
    GADTConBuilder,
    newGADTCon,

    -- * Functions
    addField,
    addField',
) where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Datatype.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type GADTConBuilder = ConstBuilder GadtC

-- | Builds a GADT constructor
newGADTCon ::
    -- | The name of the constructor
    String ->
    -- | The return type of the constructor
    TH.Type ->
    GADTConBuilder () ->
    TH.Q GadtC
newGADTCon conN t b = runBaseBuilder b (MkGadtC [TH.mkName conN] [] t)

-- | Add a field to the constructor
addField :: TH.Type -> GADTConBuilder ()
addField t = addField' (defaultBang, t)

-- | Same as 'addField', but allow setting the field's 'Kind'
addField' :: (TH.Bang, TH.Type) -> GADTConBuilder ()
addField' bt = bts |>= bt
