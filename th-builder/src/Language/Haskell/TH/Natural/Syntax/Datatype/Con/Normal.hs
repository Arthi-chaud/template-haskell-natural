-- | 'Builder' for normal constructors
module Language.Haskell.TH.Natural.Syntax.Datatype.Con.Normal (
    -- * Type
    newCon,
    ConBuilder,

    -- * Functions
    addField,
    addField',
) where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Datatype.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type ConBuilder = ConstBuilder NormalC

-- | Builds a normal constructor
newCon :: String -> ConBuilder () -> TH.Q NormalC
newCon conN b = runBaseBuilder b (MkNormalC (TH.mkName conN) [])

-- | Add a field to the constructor
addField :: TH.Type -> ConBuilder ()
addField t = addField' (defaultBang, t)

-- | Same as 'addField', but allow setting the field's 'Kind'
addField' :: (TH.Bang, TH.Type) -> ConBuilder ()
addField' bt = bts |>= bt
