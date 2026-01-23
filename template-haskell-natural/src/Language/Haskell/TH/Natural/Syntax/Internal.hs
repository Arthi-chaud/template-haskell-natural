module Language.Haskell.TH.Natural.Syntax.Internal where

import Control.Monad.State
import qualified Language.Haskell.TH as TH

-- | Common type for anything that builds a TH AST.
type Builder s a = StateT s TH.Q a

-- | Represents an identifier name
newtype Name = MkName {unN :: TH.Name}

newtype TypeVarName = MkTVName {unTVN :: Name}

-- TODO How to be able to pass a tyvarname to splicer
