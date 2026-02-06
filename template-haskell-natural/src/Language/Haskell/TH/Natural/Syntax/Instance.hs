module Language.Haskell.TH.Natural.Syntax.Instance (
    -- * Builder
    newInstance,
    InstanceDefinition,
    InstanceBuilder,

    -- * Functions
    setOverlap,
    addInstanceArg,

    -- * Re-export
    newTypeVar,
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Lens
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Internal.Name
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.QBuilder
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

-- | Add an type argument to the instance
addInstanceArg :: (QBuilder t TH.Type) => t -> InstanceBuilder ()
addInstanceArg qty = do
    ty' <- liftB $ gen qty
    unsafeWithState $ ty %= (`TH.AppT` ty')
