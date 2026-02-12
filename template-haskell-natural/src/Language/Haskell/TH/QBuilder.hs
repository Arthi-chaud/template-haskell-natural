{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Haskell.TH.QBuilder (QBuilder (..)) where

import Data.Constructor.Extract (ExtractedConstructor)
import Data.Constructor.Extract.Class (ExtractedConstructor (fromEC))
import Data.List (singleton)
import Language.Haskell.TH

-- | A QBuilder is a computation (a) or a value that can be turned into a computation in the 'Q' monad that produces a value of type b.
class QBuilder a b where
    gen :: a -> Q b

instance QBuilder a a where
    gen = pure

instance (Quote m, m ~ Q) => QBuilder (Code m a) (TExp a) where
    gen = examineCode

instance (ExtractedConstructor a Dec) => QBuilder (Q a) [Dec] where
    gen = fmap (singleton . fromEC)

-- TODO Does not work when using quotes
-- instance QBuilder (Q a) a where
instance (Quote m, m ~ Q) => QBuilder (m a) a where
    gen = id

instance (ExtractedConstructor a b) => QBuilder (Q a) b where
    gen = fmap fromEC

instance (ExtractedConstructor a b) => QBuilder a b where
    gen = pure . fromEC
