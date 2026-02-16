{-# LANGUAGE QualifiedDo #-}

module Curry (genCurries) where

import Control.Monad as M
import Data.Constructor.Extract
import Language.Haskell.TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Simple as E
import Language.Haskell.TH.Natural.Syntax.Func (bodyFromExp, newFunc, setSignature)
import Language.Haskell.TH.Natural.Syntax.Signature
import qualified Language.Haskell.TH.Natural.Syntax.Signature as S

-- https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial

-- | Generates an expression to curry a function.
--
-- If the input is 2, the produced function is
--
-- @
--  \f a1 a2 -> f (a1, a2)
-- @
curryN :: Int -> Q Exp
curryN n = genExpr $ newExpr $ E.do
    f <- arg
    args <- replicateM n arg
    returns $ f `AppE` TupE (fmap Just args)

curryNSig :: Int -> Q Type
curryNSig n = genTy $ newSignature $ S.do
    tupTys <- replicateM n (fromEC <$> newTypeVar "a")

    fResType <- newTypeVar "b"
    addParam $ newSignature $ S.do
        addParam $ foldl AppT (TupleT n) tupTys
        setResultType fResType

    forM_ tupTys addParam
    setResultType fResType

genCurries :: Int -> Q [Dec]
genCurries n = fmap concat $ forM [2 .. n] $ \i ->
    newFunc ("curry" ++ show i) $ do
        setSignature $ curryNSig i
        bodyFromExp $ curryN i
