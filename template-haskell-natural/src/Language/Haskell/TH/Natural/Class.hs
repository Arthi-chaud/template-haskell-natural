module Language.Haskell.TH.Natural.Class (THBuilder (..)) where

import Language.Haskell.TH

-- | Class for objects that can build Template Haskell ASTs in the 'Q' monad
class THBuilder a b | a -> b where
    gen :: a -> Q b

instance THBuilder (Q a) a where
    gen = id
