module Language.Haskell.TH.Natural.Syntax.Internal where

import qualified Language.Haskell.TH as TH

-- | Used for type-level tracking of the state/readiness of an AST-building computation
data ASTBuilderState a = Empty | Step a | Ready deriving (Eq, Show)

-- | Represents an identifier name
newtype Name = MkName {unN :: TH.Name}

newtype TypeVarName = MkTVName {unTVN :: Name}

-- TODO How to be able to pass a tyvarname to splicer
