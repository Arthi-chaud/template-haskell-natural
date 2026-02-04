{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.TH.Quotable (Quotable (..)) where

import Data.Constructor.Extract
import Language.Haskell.TH
import Language.Haskell.TH.QBuilder

-- | A 'Quotable' is a value that can be turned into a 'Q' computation, allowing it to be used in a quasi-quote
--
-- Identical to 'QBuilder' but the method's name is different
class Quotable a b where
    q :: a -> Q b

instance (ExtractedConstructor a b) => Quotable (Q a) b where
    q = fmap fromExtractedCon

instance (QBuilder a b) => Quotable a b where
    q = gen
