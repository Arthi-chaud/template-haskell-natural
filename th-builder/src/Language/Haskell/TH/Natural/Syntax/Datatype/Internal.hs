module Language.Haskell.TH.Natural.Syntax.Datatype.Internal (defaultBang) where

import qualified Language.Haskell.TH as TH

-- | Default 'Bang' used when adding a field to a constructor
defaultBang :: TH.Bang
defaultBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
