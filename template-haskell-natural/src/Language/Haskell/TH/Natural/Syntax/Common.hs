module Language.Haskell.TH.Natural.Syntax.Common (addContext, addBody, addBody') where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder (Builder, liftB)
import Language.Haskell.TH.Syntax.ExtractedCons

-- | Add a constraint to a context
addContext :: (HasCxt s [a]) => TH.Q a -> Builder s step step ()
addContext qty = do
    ty_ <- liftB qty
    cxt |>= ty_

-- | Add a 'Dec'
addBody :: (HasDecs s [TH.Dec], GenDec a) => a -> Builder s step step ()
addBody s = do
    dec <- liftB $ genDec s
    decs |>= dec

-- | Add many 'Dec's
addBody' :: (HasDecs s [a]) => TH.Q [a] -> Builder s step step ()
addBody' s = do
    dec' <- liftB s
    decs <>= dec'
