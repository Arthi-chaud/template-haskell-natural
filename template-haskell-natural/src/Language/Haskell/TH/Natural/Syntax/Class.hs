{-# LANGUAGE RankNTypes #-}

module Language.Haskell.TH.Natural.Syntax.Class (
    -- * Builder
    newClass,
    ClassDefinition,
    ClassBuilder,

    -- * Functions
    addTypeVar,
    addFunDep,
    addSignature,

    -- * Re-export
    newTypeVar,
) where

import Control.Lens
import Language.Haskell.TH hiding (cxt, funDep)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Internal.Name
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.QBuilder
import Language.Haskell.TH.Syntax.ExtractedCons hiding (fName)

type ClassDefinition = Q ClassD

type ClassBuilder a = ConstBuilder ClassD a

-- | Starts the building of a class declaration, using its name and a 'ClassBuilder'
newClass :: String -> ClassBuilder () -> ClassDefinition
newClass className next = runBaseBuilder next class_
  where
    class_ = MkClassD [] (mkName className) [] [] []

-- | Add the given 'TypeVar' to the class' arguments
addTypeVar :: TypeVarName -> BndrVis -> Maybe TH.Kind -> ClassBuilder ()
addTypeVar tyN vis mkind =
    tyVarBndr |>= maybe (PlainTV n vis) (KindedTV n vis) mkind
  where
    n = tyN ^. name

-- | Add functional dependencies
addFunDep :: [TypeVarName] -> [TypeVarName] -> ClassBuilder ()
addFunDep l r = funDep |>= FunDep (fmap (^. name) l) (fmap (^. name) r)

-- | Add a function signature to the class
addSignature :: (QBuilder a TH.Type) => String -> a -> ClassBuilder ()
addSignature fName tyBuilder = do
    sigTy <- liftB $ gen tyBuilder
    addBody $ pure $ TH.SigD (mkName fName) sigTy
