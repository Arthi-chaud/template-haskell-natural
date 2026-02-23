{-# LANGUAGE RankNTypes #-}

-- | 'Builder' for declaring typeclass
module Language.Haskell.TH.Natural.Syntax.Class (
    -- * Builder
    newClass,
    ClassBuilder,

    -- * Functions
    addTypeVar,
    addTypeVar',
    addFunDep,
    addSignature,

    -- * Re-export
    newTypeVar,
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Lens
import Language.Haskell.TH hiding (cxt, funDep)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Natural.Syntax.Name
import Language.Haskell.TH.Syntax.ExtractedCons hiding (fName)

type ClassBuilder a = ConstBuilder ClassD a

-- | Builds a typeclass declaration, using its name and a 'ClassBuilder'
newClass :: String -> ClassBuilder () -> Q ClassD
newClass className next = runBaseBuilder next class_
  where
    class_ = MkClassD [] (mkName className) [] [] []

-- | Add functional dependencies
addFunDep :: [TypeVarName] -> [TypeVarName] -> ClassBuilder ()
addFunDep l r = funDep |>= FunDep (fmap (^. name) l) (fmap (^. name) r)

-- | Add a function signature to the class
addSignature :: (GenType a) => String -> a -> ClassBuilder ()
addSignature fName tyBuilder = do
    sigTy <- liftB $ genTy tyBuilder
    addBody $ TH.SigD (mkName fName) sigTy
