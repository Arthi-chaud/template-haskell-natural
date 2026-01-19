{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons where

import Data.Constructor.Extract
import GHC.Generics
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.TH.Syntax as TH

extractConstructorsOf ''Exp defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
type Match = TH.Match
type Clause = TH.Clause
extractConstructorsOf ''Body defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Guard defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Pat defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Range defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Lit defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''TyLit defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Stmt defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Con defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Type defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
extractConstructorsOf ''Dec defaultOptions{deriveClasses = [''Generic, ''Eq, ''Show]}
