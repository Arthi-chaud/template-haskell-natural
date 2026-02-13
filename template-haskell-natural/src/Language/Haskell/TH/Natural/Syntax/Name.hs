module Language.Haskell.TH.Natural.Syntax.Name (TypeVarName, newTypeVar) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Syntax.ExtractedCons (VarT (MkVarT))

type TypeVarName = VarT

newTypeVar :: String -> Builder st step step TypeVarName
newTypeVar n = liftB (MkVarT <$> TH.newName n)
