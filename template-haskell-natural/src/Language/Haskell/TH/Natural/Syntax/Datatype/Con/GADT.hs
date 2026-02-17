module Language.Haskell.TH.Natural.Syntax.Datatype.Con.GADT (
    -- * Type
    GADTConDefinition,
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

type GADTConDefinition = TH.Q GadtC

type GADTConBuilder = ConstBuilder GadtC

newGADTCon ::
    -- | The name of the constructor
    String ->
    -- | The return type of the constructor
    TH.Type ->
    GADTConBuilder () ->
    GADTConDefinition
newGADTCon conN t b = runBaseBuilder b (MkGadtC [TH.mkName conN] [] t)

addField :: TH.Type -> GADTConBuilder ()
addField t = addField' t defaultBang

addField' :: TH.Type -> TH.Bang -> GADTConBuilder ()
addField' t b = bts |>= (b, t)
