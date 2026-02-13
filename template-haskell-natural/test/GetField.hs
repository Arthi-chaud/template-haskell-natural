{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module GetField where

import Language.Haskell.TH
import Language.Haskell.TH.Natural.Syntax.Expr.Simple as E
import Language.Haskell.TH.Natural.Syntax.Func as F

pattern Pair :: a -> b -> (a, b)
pattern Pair x y <- (x, y)
    where
        Pair x y = (x, y)

{-# COMPLETE Pair #-}

data InfixSum = Int :+: Int

newDeclarationGroup

mkFst :: FuncDefinition
mkFst = newFunc "fstPair" $ F.do
    setSignature [t|forall a b. (a, b) -> a|]
    bodyFromExp $ newExpr $ E.do
        pair <- arg
        a <- getField 'Pair 0 pair
        returns a

mkGetSumLeft :: FuncDefinition
mkGetSumLeft = newFunc "getSumLeft" $ F.do
    setSignature [t|InfixSum -> Int|]
    bodyFromExp $ newExpr $ E.do
        isum <- arg
        a <- getField '(:+:) 0 isum
        returns a
