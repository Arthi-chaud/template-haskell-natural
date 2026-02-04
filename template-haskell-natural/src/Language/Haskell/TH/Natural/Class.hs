{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Haskell.TH.Natural.Class (THBuilder (..), q) where

import Data.Constructor.Extract.Class (ExtractedConstructor (fromExtractedCon))
import Language.Haskell.TH

-- | Class for objects that can build Template Haskell ASTs in the 'Q' monad
class THBuilder a b where
    gen :: a -> Q b

-- TODO Does not work when using quotes
-- instance THBuilder (Q a) a where
instance (Quote m, m ~ Q) => THBuilder (m a) a where
    gen = id

instance (ExtractedConstructor a Dec) => THBuilder (Q a) Dec where
    gen = fmap fromExtractedCon

instance (ExtractedConstructor a Type) => THBuilder (Q a) Type where
    gen = fmap fromExtractedCon

instance (ExtractedConstructor a Exp) => THBuilder (Q a) Exp where
    gen = fmap fromExtractedCon

instance THBuilder a a where
    gen = pure

q :: a -> Q a -- TODO Move me
q = return
