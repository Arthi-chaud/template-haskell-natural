{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Haskell.TH.QBuilder (QBuilder (..), q) where

import Data.Constructor.Extract.Class (ExtractedConstructor (fromExtractedCon))
import Language.Haskell.TH

-- | A QBuilder is a computation (a) or a value that can be turned into a computation in the 'Q' monad that produces a value of type b.
class QBuilder a b where
    gen :: a -> Q b

instance QBuilder a a where
    gen = pure

-- TODO Does not work when using quotes
-- instance QBuilder (Q a) a where
instance (Quote m, m ~ Q) => QBuilder (m a) a where
    gen = id

instance (ExtractedConstructor a Dec) => QBuilder (Q a) Dec where
    gen = fmap fromExtractedCon

instance (ExtractedConstructor a Type) => QBuilder (Q a) Type where
    gen = fmap fromExtractedCon

instance (ExtractedConstructor a Exp) => QBuilder (Q a) Exp where
    gen = fmap fromExtractedCon

-- istance (ExtractedConstructor a b) => QBuilder (Q a) b where
--     gen = fmap fromExtractedCon

q :: a -> Q a -- TODO Move me
q = return
