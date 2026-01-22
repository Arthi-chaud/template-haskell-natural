module Language.Haskell.TH.Natural.Syntax.Class (
    -- * Types
    ClassDefinition,
    ClassBuilder,

    -- * Variable binding
    newTypeVar,

    -- * Class
    newClass,
    addContext,
    addTypeVar,
    addFunDep,
    addBody,
    addSignature,
) where

import Control.Lens
import Control.Monad.State
import Data.Coerce (coerce)
import Language.Haskell.TH hiding (cxt, funDep)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type ClassDefinition = Q ClassD

type ClassBuilder a = StateT ClassD Q a

-- | Binds a new type variable to be used across the class definition
newTypeVar :: ClassBuilder TypeVarName
newTypeVar = fmap coerce $ lift $ newName "n"

-- | Starts the building of a class declaration, using its name and a 'ClassBuilder'
newClass :: String -> ClassBuilder () -> ClassDefinition
newClass className next = execStateT next class_
  where
    class_ = MkClassD [] (mkName className) [] [] []

-- | Add a constraint to the class' context
addContext :: Q TH.Type -> ClassBuilder ()
addContext qty = do
    ty_ <- lift qty
    cxt %= (++ [ty_])

-- | Add the given 'TypeVar' to the class' arguments
addTypeVar :: TypeVarName -> BndrVis -> Maybe TH.Kind -> ClassBuilder ()
addTypeVar tyN vis mkind =
    tyVarBndr %= (maybe (PlainTV n vis) (KindedTV n vis) mkind :)
  where
    n = coerce tyN

-- | Add functional dependencies
addFunDep :: [TypeVarName] -> [TypeVarName] -> ClassBuilder ()
addFunDep l r = funDep %= (++ [FunDep (fmap coerce l) (fmap coerce r)])

-- | Add a 'Dec' to the class' body
addBody :: (THBuilder s Dec) => s -> ClassBuilder ()
addBody s = do
    dec <- lift $ gen s
    decs %= (++ [dec])

-- | Alias to 'addBody'
addSignature :: Q TH.Dec -> ClassBuilder ()
addSignature = addBody
