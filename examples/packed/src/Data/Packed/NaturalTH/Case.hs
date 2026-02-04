{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.Packed.NaturalTH.Case (genCase) where

import Control.Applicative (liftA3)
import Control.Monad
import Data.Packed
import Data.Packed.TH.Utils (getBranchesTyList, resolveAppliedType)
import Language.Haskell.TH
import Language.Haskell.TH.Natural.Class
import Language.Haskell.TH.Natural.Syntax.Case
import Language.Haskell.TH.Natural.Syntax.Expr.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Do
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Internal.Builder
import qualified Language.Haskell.TH.Natural.Syntax.Internal.Builder as B
import Language.Haskell.TH.Natural.Syntax.Signature

genCase :: Name -> DecsQ
genCase tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    newFunc ("case" ++ nameBase tyName) $ do
        setSignature $ newSignature caseSignature
        bodyFromExp $ newExpr $ B.do
            args <- forM [0 .. length cs - 1] $ const arg
            returns [|mkPackedReader $(newExpr $ caseBody args)|]
  where
    caseSignature = B.do
        (sourceType, _) <- liftB $ resolveAppliedType tyName
        branchesTypes <- liftB $ getBranchesTyList tyName []
        r <- q . VarT <$> liftB (newName "r")
        b <- q . VarT <$> liftB (newName "b")
        forM_ branchesTypes $ \branchType -> B.do
            addParam $
                let branchTypeList = q $ foldr (\a rest -> ConT '(:) `AppT` a `AppT` rest) (ConT '[]) branchType
                 in [t|PackedReader $branchTypeList $r $b|]
        setResultType [t|PackedReader '[$(return sourceType)] $r $b|]
    caseBody caseReaders = B.do
        packed <- q <$> arg
        l <- q <$> arg
        returns $ newDo $ B.do
            tpl <- bind [|runReader reader $packed $l|]
            (tag, packed1, l1) <- liftA3 (,,) (getField '(,,) 0 tpl) (getField '(,,) 1 tpl) (getField '(,,) 2 tpl)
            returns $ case_ tag $ B.do
                forM_ ([0 ..] `zip` caseReaders) $ \(i, caseReader) ->
                    matchConst (litP $ IntegerL i) [|runReader $(q caseReader) $(q packed1) $(q l1)|]
                matchWild [|fail "Bad Tag"|]
