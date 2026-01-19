{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Lenses where

import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons
import Language.Haskell.TH.Syntax.ExtractedCons.Internal

-- TODO For each extracted cons, for each field, generate lens using position

mkExtractedConsLens ''VarE "name" 0
mkExtractedConsLens ''ConE "name" 0
