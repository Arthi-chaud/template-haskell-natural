{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Do.State where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Expr.Internal

data DoExprStep = Bind Binding | Stmt TH.Exp | Let Binding | Decons Deconstruct

newtype DoExprBuilderState
    = -- | Works as a stack: the last step is the first in the list
      MkDoEBS {_steps :: [DoExprStep]}

makeLenses ''DoExprBuilderState
