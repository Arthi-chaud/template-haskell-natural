-- | This package exposes lenses for most of the ADTs from the Template Haskell API.
--
-- It uses the 'extract-cons' and'lens-adt' to extract all the constructors and generate lenses respectively.
module Language.Haskell.TH.Syntax.ExtractedCons (
    module Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons,
    module Language.Haskell.TH.Syntax.ExtractedCons.Lenses,
) where

import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons
import Language.Haskell.TH.Syntax.ExtractedCons.Lenses
