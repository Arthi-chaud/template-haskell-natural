-- | 'Builder' for instance declaration
module Language.Haskell.TH.Natural.Syntax.Instance (
    -- * Builder
    newInstance,
    InstanceBuilder,

    -- * Functions
    setOverlap,
    addInstanceArg,

    -- * Re-export
    newTypeVar,
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
    module Language.Haskell.TH.Natural.Syntax.Common,
) where

import Control.Lens
import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Natural.Syntax.Name
import Language.Haskell.TH.Syntax.ExtractedCons

type InstanceBuilder a = ConstBuilder InstanceD a

-- | Builds a new instance for the typeclass. The argument is the 'Name' of that typeclass
newInstance :: TH.Name -> InstanceBuilder () -> Q InstanceD
newInstance className builder = runBaseBuilder builder instance_
  where
    instance_ = MkInstanceD Nothing [] (TH.ConT className) []

-- | Set an 'Overlap' pragma to the instance
setOverlap :: TH.Overlap -> InstanceBuilder ()
setOverlap = (overlap ?=)

-- | Add an type argument to the instance
addInstanceArg :: (GenType t) => t -> InstanceBuilder ()
addInstanceArg qty = do
    ty' <- liftB $ genTy qty
    unsafeWithState $ ty %= (`TH.AppT` ty')
