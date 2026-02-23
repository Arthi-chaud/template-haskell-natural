-- | This package allows _extracting_ constructors from a given ADT.
--
-- @
-- data MathE = Add MathE MathE | ...
--
-- \$('extractConstructorsOf' 'MathE defaultOptions)
--
-- -- ==> The following gets generated, for each constructor
-- data Add = MkAdd MathE MathE
--
-- instance 'ExtractedConstructor' Add MathE where
--  'fromEC' (MkAdd left right) = Add left right
--  'toEC' mathE = case mathE of
--      (Add left right) -> Just (MkAdd left right)
--      _ -> Nothing
-- @
module Data.Constructor.Extract (
    -- * Extract Constructors
    extractConstructor,
    extractConstructorsOf,

    -- * Options
    ExtractOptions (..),
    defaultOptions,

    -- * Conversion
    ExtractedConstructor (..),
) where

import Data.Constructor.Extract.Class
import Data.Constructor.Extract.Options
import Data.Constructor.Extract.TH
