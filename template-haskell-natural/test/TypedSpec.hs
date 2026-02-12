{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TypedSpec (spec) where

import Language.Haskell.TH.Natural.Syntax.Expr.Simple.Typed as T
import Language.Haskell.TH.Quotable
import Test.Hspec

mkSimpleAdd :: SimpleTypedExprDefinition (Int -> Int -> Int)
mkSimpleAdd = T.newExpr $ T.do
    left <- arg
    right <- arg
    returns [||$$(qT left) + $$(qT right)||]

spec :: Spec
spec = pure ()
