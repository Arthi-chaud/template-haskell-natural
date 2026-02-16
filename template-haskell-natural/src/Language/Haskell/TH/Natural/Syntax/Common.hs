module Language.Haskell.TH.Natural.Syntax.Common (addContext, addBody, addBody', addTypeVar, addTypeVar') where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder (Builder, liftB)
import Language.Haskell.TH.Natural.Syntax.Name
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

addTypeVar :: (HasTyVarBndr s [TH.TyVarBndr TH.BndrVis]) => TypeVarName -> Builder s step step ()
addTypeVar tyN = addTypeVar' tyN TH.BndrReq Nothing

addTypeVar' :: (HasTyVarBndr s [TH.TyVarBndr vis]) => TypeVarName -> vis -> Maybe TH.Kind -> Builder s step step ()
addTypeVar' tyN vis mkind = tyVarBndr |>= maybe (TH.PlainTV n vis) (TH.KindedTV n vis) mkind
  where
    n = tyN ^. name
