{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Simple.State where

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Expr.Internal

data SimpleExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _lets :: [Binding]
    , _deconstructs :: [Deconstruct]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''SimpleExprBuilderState
