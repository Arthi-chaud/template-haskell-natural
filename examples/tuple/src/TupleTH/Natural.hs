{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}

module TupleTH.Natural where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.List ((!?))
import Language.Haskell.TH
import Language.Haskell.TH.Gen (GenExpr (genExpr))
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Case (case_, matchList, matchWild)
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Quotable

type TupleSize = Int
type TupleFieldIndex = Int

mapTuple :: TupleSize -> ExpQ
mapTuple n = genExpr $ newExpr $ B.do
    f <- arg
    tupFields <- tupleFields n =<< arg
    let transformedTupleFields = (f `AppE`) <$> tupFields
    returns $ tupE (pure <$> transformedTupleFields)

safeTupleFromList :: TupleSize -> ExpQ
safeTupleFromList n = genExpr $ newExpr $ B.do
    list <- arg
    returns $ case_ list $ B.do
        matchList n $ \items -> [|Just $(tupE $ pure <$> items)|]
        matchWild [|Nothing|]

filterTuple :: TupleSize -> ExpQ
filterTuple n = genExpr $ newExpr $ B.do
    f <- arg
    tupFields <- tupleFields n =<< arg
    returns [|filter $(q f) $(listE (pure <$> tupFields))|]

reverseTuple :: TupleSize -> ExpQ
reverseTuple n = genExpr $ newExpr $ B.do
    tupFields <- tupleFields n =<< arg
    returns (tupE (pure <$> reverse tupFields))

deleteAtTuple :: TupleSize -> TupleFieldIndex -> ExpQ
deleteAtTuple n idx = genExpr $ newExpr $ B.do
    tup <- arg
    tupFields <- getTupleFields n tup
    let filteredFields = uncurry (++) $ second (drop 1) $ splitAt idx tupFields
    returns (tupE (pure <$> filteredFields))

orTuple :: TupleSize -> ExpQ
orTuple n = foldTuple n [|False|] [|(||)|]

andTuple :: TupleSize -> ExpQ
andTuple n = foldTuple n [|True|] [|(&&)|]

sumTuple :: TupleSize -> ExpQ
sumTuple n = foldTuple n [|0|] [|(+)|]

anyTuple :: TupleSize -> ExpQ
anyTuple n = genExpr $ newExpr $ B.do
    f <- arg
    tupFields <- tupleFields n =<< arg
    returns [|any $(q f) $(listE (pure <$> tupFields))|]

foldTuple :: TupleSize -> ExpQ -> ExpQ -> ExpQ
foldTuple n deflt joinF = genExpr $ newExpr $ B.do
    tupFields <- tupleFields n =<< arg
    returns $
        if n == 0
            then deflt
            else foldl (\rest item -> [|$joinF $(q item) $rest|]) deflt tupFields

elemTuple :: TupleSize -> ExpQ
elemTuple n = genExpr $ newExpr $ B.do
    item <- arg
    tup <- arg
    returns [|$(anyTuple n) (== $(q item)) $(q tup)|]

catTuples :: TupleSize -> TupleSize -> ExpQ
catTuples n1 n2 = genExpr $ newExpr $ B.do
    leftTup <- arg
    rightTup <- arg
    tup1 <- tupleFields n1 leftTup
    tup2 <- tupleFields n2 rightTup
    let fields = tup1 ++ tup2
    returns $ tupE (pure <$> fields)

zipTuple :: TupleSize -> TupleSize -> ExpQ
zipTuple n1 n2 = genExpr $ newExpr $ B.do
    tup1 <- tupleFields n1 =<< arg
    tup2 <- tupleFields n2 =<< arg
    let zippedFields = zipWith (\a b -> tupE [pure a, pure b]) tup1 tup2
    returns $ listE zippedFields

proj :: TupleSize -> TupleFieldIndex -> ExpQ
proj size idx = genExpr $ newExpr $ B.do
    tupFields <- tupleFields size =<< arg
    case tupFields !? idx of
        Nothing -> B.fail "Out-of-bounds access of tuple field"
        Just f -> returns f

updateAtTuple :: TupleSize -> TupleFieldIndex -> ExpQ
updateAtTuple size idx = genExpr $ newExpr $ B.do
    f <- arg
    tupFields <- tupleFields size =<< arg
    let updated =
            ([0 ..] `zip` tupFields) <&> \(i, field) ->
                if i == idx
                    then f `AppE` field
                    else field
    returns $ tupE (pure <$> updated)

tupleFields :: (GenExpr b, IsExprBuilder st) => TupleSize -> b -> Builder st Empty Empty [Exp]
tupleFields n b = B.do
    e <- liftB $ genExpr b
    forM [0 .. n - 1] $ \i -> getTupleField n i e
