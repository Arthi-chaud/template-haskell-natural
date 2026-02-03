module Language.Haskell.TH.Natural.Syntax.Internal.Name (Name (..), TypeVarName (..), newTypeVar) where

import Data.Coerce
import qualified Language.Haskell.TH as TH

-- | Represents an identifier name
newtype Name = MkName {unN :: TH.Name}

newtype TypeVarName = MkTVName {unTVN :: Name}

-- | Binds a new type variable to be used across the class definition
newTypeVar :: String -> TH.Q TypeVarName
newTypeVar n = coerce <$> TH.newName n

-- TODO How to be able to pass a tyvarname to splicer
