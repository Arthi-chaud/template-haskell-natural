module Language.Haskell.TH.Natural.Syntax.Internal.Name (Name (..), TypeVarName (..)) where

import qualified Language.Haskell.TH as TH

-- | Represents an identifier name
newtype Name = MkName {unN :: TH.Name}

newtype TypeVarName = MkTVName {unTVN :: Name}

-- TODO How to be able to pass a tyvarname to splicer
