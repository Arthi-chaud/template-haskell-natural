module Language.Haskell.TH.Natural.Syntax.Datatype.Con.Normal (
    -- * Type
    ConDefinition,
    ConBuilder,
    newCon,

    -- * Functions
    addField,
    addField',
) where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Datatype.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type ConDefinition = TH.Q NormalC

type ConBuilder = ConstBuilder NormalC

newCon :: String -> ConBuilder () -> ConDefinition
newCon conN b = runBaseBuilder b (MkNormalC (TH.mkName conN) [])

addField :: TH.Type -> ConBuilder ()
addField t = addField' (defaultBang, t)

addField' :: (TH.Bang, TH.Type) -> ConBuilder ()
addField' bt = bts |>= bt
