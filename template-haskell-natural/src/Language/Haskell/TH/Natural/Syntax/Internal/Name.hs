module Language.Haskell.TH.Natural.Syntax.Internal.Name (VarName, newVar, TypeVarName, newTypeVar) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax.ExtractedCons (VarE (MkVarE), VarT (MkVarT))

type VarName = VarE

-- | Binds a new type variable to be used across the class definition
newVar :: String -> TH.Q VarName
newVar n = MkVarE <$> TH.newName n

type TypeVarName = VarT

-- | Binds a new type variable to be used across the class definition
newTypeVar :: String -> TH.Q TypeVarName
newTypeVar n = MkVarT <$> TH.newName n

-- TODO How to be able to pass a tyvarname to splicer
