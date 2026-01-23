module Language.Haskell.TH.Natural.Syntax.Common (addContext, addBody, addBody') where

import Control.Lens
import Control.Monad.State
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons

-- | Add a constraint to a context
addContext :: (HasCxt s [TH.Type]) => TH.Q TH.Type -> Builder s ()
addContext qty = do
    ty_ <- lift qty
    cxt %= (++ [ty_])

-- | Add a 'Dec'
addBody :: (HasDecs s [TH.Dec]) => TH.Q TH.Dec -> Builder s ()
addBody s = do
    dec <- lift s
    decs %= (++ [dec])

-- | Add many 'Dec's
addBody' :: (HasDecs s [TH.Dec]) => [TH.Q TH.Dec] -> Builder s ()
addBody' s = do
    dec' <- lift $ sequence s
    decs %= (++ dec')
