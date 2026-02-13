{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

module PatSyn where

import Language.Haskell.TH (newDeclarationGroup)
import Language.Haskell.TH.Natural.Syntax.Expr.Simple as E
import Language.Haskell.TH.Natural.Syntax.Func as F

pattern Pair :: a -> b -> (a, b)
pattern Pair x y <- (x, y)
    where
        Pair x y = (x, y)

{-# COMPLETE Pair #-}

newDeclarationGroup

mkFst :: FuncDefinition
mkFst = newFunc "fstPair" $ F.do
    setSignature [t|forall a b. (a, b) -> a|]
    bodyFromExp $ newExpr $ E.do
        pair <- arg
        a <- getField 'Pair 0 pair
        returns a
