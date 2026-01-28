{-# LANGUAGE RankNTypes #-}

module Language.Haskell.TH.Natural.Syntax.Class (
    -- * Types
    ClassDefinition,
    ClassBuilder,

    -- * Variable binding
    newTypeVar,

    -- * Functions
    newClass,
    addTypeVar,
    addFunDep,
    addSignature,
) where

import Control.Lens
import Control.Monad.State
import Data.Coerce (coerce)
import Language.Haskell.TH hiding (cxt, funDep)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder, gen)
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type ClassDefinition = Q ClassD

type ClassBuilder a = ConstBuilder ClassD a

-- | Starts the building of a class declaration, using its name and a 'ClassBuilder'
newClass :: String -> ClassBuilder () -> ClassDefinition
newClass className next = runBaseConstBuilder next class_
  where
    class_ = MkClassD [] (mkName className) [] [] []

-- | Add the given 'TypeVar' to the class' arguments
addTypeVar :: TypeVarName -> BndrVis -> Maybe TH.Kind -> ClassBuilder ()
addTypeVar tyN vis mkind =
    tyVarBndr %= (maybe (PlainTV n vis) (KindedTV n vis) mkind :)
  where
    n = coerce tyN

-- | Add functional dependencies
addFunDep :: [TypeVarName] -> [TypeVarName] -> ClassBuilder ()
addFunDep l r = funDep %= (++ [FunDep (fmap coerce l) (fmap coerce r)])

-- | Add a function signature to the class
addSignature :: (THBuilder a TH.Type) => String -> a -> ClassBuilder ()
addSignature fName tyBuilder = do
    sigTy <- lift $ gen tyBuilder
    addBody $ pure $ TH.SigD (mkName fName) sigTy
