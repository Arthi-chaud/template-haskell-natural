module Language.Haskell.TH.Quotable (Quotable (..), qEC, qT) where

import Data.Constructor.Extract
import Language.Haskell.TH

-- | A 'Quotable' is a value that can be turned into a 'Q' computation, allowing it to be used in a quasi-quote
class Quotable a b where
    q :: a -> Q b

instance Quotable (Q a) a where
    q = id

instance Quotable a a where
    q = pure

qEC :: (ExtractedConstructor a b) => a -> Q b
qEC = pure . fromEC

qT :: TExp a -> Code Q a
qT = Code . pure
