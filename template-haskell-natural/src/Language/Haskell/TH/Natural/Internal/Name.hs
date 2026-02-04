module Language.Haskell.TH.Natural.Internal.Name (VarName, newVar, TypeVarName, newTypeVar) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax.ExtractedCons (VarE (MkVarE), VarT (MkVarT))

type VarName = VarE

newVar :: String -> TH.Q VarName
newVar n = MkVarE <$> TH.newName n

type TypeVarName = VarT

newTypeVar :: String -> TH.Q TypeVarName
newTypeVar n = MkVarT <$> TH.newName n
