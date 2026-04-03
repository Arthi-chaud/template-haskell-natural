{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TypedSpec (spec) where

import Language.Haskell.TH.Natural.Syntax.Expr.Simple.Typed as T
import Language.Haskell.TH.Quotable
import Test.Hspec

mkTypedE :: SimpleTypedExprDefinition (Int -> String -> String)
mkTypedE = T.newExpr $ T.do
    count <- arg
    str <- arg
    returns [||concat $ replicate $$(qT count) $$(qT str)||]

spec :: Spec
spec = pure ()
