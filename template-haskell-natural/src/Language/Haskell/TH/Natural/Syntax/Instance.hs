module Language.Haskell.TH.Natural.Syntax.Instance (
    -- * Builder
    newInstance,
    InstanceDefinition,
    InstanceBuilder,

    -- * Functions
    setOverlap,
    unsetOverlap,
    addInstanceArg,

    -- * Re-export
    newTypeVar,
) where

import Control.Lens
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

type InstanceDefinition = Q InstanceD

type InstanceBuilder a = ConstBuilder InstanceD a

newInstance :: TH.Name -> InstanceBuilder () -> InstanceDefinition
newInstance className builder = runBaseBuilder builder instance_
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
    ty' <- liftB qty
    unsafeWithState $ ty %= (`TH.AppT` ty')
