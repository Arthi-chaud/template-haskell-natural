module Language.Haskell.TH.Natural.Syntax.Instance (
    -- * Types
    InstanceDefinition,
    InstanceBuilder,

    -- * Functions
    newInstance,
    setOverlap,
    unsetOverlap,
    addInstanceArg,
) where

import Control.Lens
import Control.Monad.State
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type InstanceDefinition = Q InstanceD

type InstanceBuilder a = ConstBuilder InstanceD a

newInstance :: TH.Name -> InstanceBuilder () -> InstanceDefinition
newInstance className builder = runBaseConstBuilder builder instance_
  where
    instance_ = MkInstanceD Nothing [] (TH.ConT className) []

-- | Set an 'Overlap' pragma to the instance
setOverlap :: TH.Overlap -> InstanceBuilder ()
setOverlap = (overlap ?=)

-- | Unset the 'Overlap' pragma associated with the instance (if any)
unsetOverlap :: InstanceBuilder ()
unsetOverlap = overlap .= (Nothing :: Maybe TH.Overlap)

-- | Add an type argument to the instance
addInstanceArg :: TH.Q TH.Type -> InstanceBuilder ()
addInstanceArg qty = do
    ty' <- lift qty
    ty %= (`TH.AppT` ty')
