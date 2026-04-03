{-# LANGUAGE QualifiedDo #-}

module Tuple (generateTupleBoilerplate, generateTupleClass, generateTupleInstance) where

import Control.Monad
import Data.Constructor.Extract
import Language.Haskell.TH
import Language.Haskell.TH.Gen
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func (bodyFromExp, newFunc)
import Language.Haskell.TH.Natural.Syntax.Instance
import Language.Haskell.TH.Natural.Syntax.Signature

-- https://serokell.io/blog/introduction-to-template-haskell#example%3A-generating-instances

generateTupleClass :: Int -> Q [Dec]
generateTupleClass n = genDecs $ newClass ("Tuple" ++ n') $ B.do
    when (n <= 0) $
        Prelude.fail $
            "Non-Positive Size: " ++ n'
    t <- newTypeVar "t"
    r <- newTypeVar "r"
    addTypeVar t
    addTypeVar r
    addFunDep [t] [r]
    addSignature ("_" ++ n') $ B.do
        addParam t
        setResultType r
  where
    n' = show n

generateTupleInstance :: Int -> Int -> Q [Dec]
generateTupleInstance element size = genDecs $ newInstance (mkName $ "Tuple" ++ element') $ B.do
    when (element > size) $
        Prelude.fail
            "Field index is larger than tuple size"
    tupTys <- replicateM size $ newTypeVar "t"
    addInstanceArg $ foldl AppT (TupleT size) (fromEC <$> tupTys)
    addInstanceArg (tupTys !! (element - 1))

    addBody' $ newFunc ("_" ++ show element) $ bodyFromExp $ newExpr $ B.do
        tup <- arg
        res <- getTupleField size (element - 1) tup
        returns res
  where
    element' = show element

generateTupleBoilerplate :: Int -> Q [Dec]
generateTupleBoilerplate n = fmap concat $ forM [2 .. n] $ \size -> do
    class_ <- generateTupleClass size
    instances <- fmap concat $ forM [2 .. size] $ \elem' -> generateTupleInstance elem' size
    return (class_ ++ instances)
