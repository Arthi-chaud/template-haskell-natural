module Language.Haskell.TH.Natural.Syntax.Common (addContext, addBody, addBody') where

import Control.Lens
import Control.Monad.State
import Language.Haskell.TH.Syntax.ExtractedCons

-- | Add a constraint to a context
addContext :: (MonadTrans t, Monad m, MonadState s (t m), HasCxt s [a]) => m a -> t m ()
addContext qty = do
    ty_ <- lift qty
    cxt |>= ty_

-- | Add a 'Dec'
addBody :: (MonadTrans t, Monad m, MonadState s (t m), HasDecs s [a]) => m a -> t m ()
addBody s = do
    dec <- lift s
    decs |>= dec

-- | Add many 'Dec's
addBody' :: (MonadTrans t, Monad m, MonadState s (t m), HasDecs s [a]) => [m a] -> t m ()
addBody' s = do
    dec' <- lift $ sequence s
    decs <>= dec'
