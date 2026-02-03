module Language.Haskell.TH.Natural.Class (THBuilder (..)) where

import Data.Constructor.Extract.Class (ExtractedConstructor (fromExtractedCon))
import Language.Haskell.TH

-- | Class for objects that can build Template Haskell ASTs in the 'Q' monad
class THBuilder a b where
    gen :: a -> Q b

instance THBuilder a a where
    gen = pure

instance THBuilder (Q a) a where
    gen = id

instance (ExtractedConstructor a Dec) => THBuilder (Q a) Dec where
    gen = fmap fromExtractedCon
