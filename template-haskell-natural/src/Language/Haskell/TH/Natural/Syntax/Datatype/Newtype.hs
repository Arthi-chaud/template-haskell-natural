module Language.Haskell.TH.Natural.Syntax.Datatype.Newtype (
    NewtypeDefinition,
    newNewtype,

    -- * Re-exports
    module Language.Haskell.TH.Natural.Syntax.Datatype.Data,
) where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Datatype.Data
import Language.Haskell.TH.Natural.Syntax.Datatype.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type NewtypeDefinition = TH.Q NewtypeD

-- | Allows defining a newtype using a 'DataBuilder'.
--
-- Will throw at compile-time (and thus interupt compilation) if the newtype
--  - has more than one constructor
--  - that constructor has more than one field
newNewtype :: String -> DataBuilder () -> NewtypeDefinition
newNewtype dataNameStr builder = do
    dataD <- newData dataNameStr builder
    case dataD ^. con of
        [c] ->
            let
                newtypeD = MkNewtypeD (dataD ^. cxt) (dataD ^. name) (dataD ^. tyVarBndr) (dataD ^. kind) c (dataD ^. derive)
             in
                case c of
                    (TH.NormalC _ [(b, _)]) | noBang b -> return newtypeD
                    (TH.RecC _ [(_, b, _)]) | noBang b -> return newtypeD
                    _ -> fail "When building a newtype, the constructor must be a normal or record constructor, without bang annotation"
        _ -> fail "When building a newtype, exactly one constructor is expected"
  where
    noBang b = b == defaultBang
