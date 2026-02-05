{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.Packed.NaturalTH.Case (genCase, caseFName) where

import Control.Applicative (liftA3)
import Control.Monad
import Data.Packed
import Data.Packed.Reader (runPackedReader)
import Data.Packed.TH (Tag)
import Data.Packed.TH.Utils (getBranchesTyList, resolveAppliedType, sanitizeConName)
import Language.Haskell.TH
import Language.Haskell.TH.Natural.Syntax.Builder hiding (fail)
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Case
import Language.Haskell.TH.Natural.Syntax.Expr.Do
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Signature
import Language.Haskell.TH.Quotable

caseFName :: Name -> Name
caseFName tyName = mkName $ "case" ++ sanitizeConName tyName

genCase :: Name -> DecsQ
genCase tyName = do
    (TyConI (DataD _ _ _ _ cs _)) <- reify tyName
    newFunc (nameBase $ caseFName tyName) $ do
        inline
        setSignature caseSignature
        bodyFromExp $ B.do
            args <- forM [0 .. length cs - 1] $ const arg
            returns [|mkPackedReader $(newExpr $ caseBody args)|]
  where
    caseSignature = B.do
        (sourceType, _) <- liftB $ resolveAppliedType tyName
        branchesTypes <- liftB $ getBranchesTyList tyName []
        r <- qCon <$> liftB (newTypeVar "r")
        b <- qCon <$> liftB (newTypeVar "b")
        forM_ branchesTypes $ \branchType -> B.do
            addParam $
                let branchTypeList = q $ foldr (\a rest -> ConT '(:) `AppT` a `AppT` rest) (ConT '[]) branchType
                 in [t|PackedReader $branchTypeList $r $b|]
        setResultType [t|PackedReader '[$(q sourceType)] $r $b|]
    caseBody :: [Exp] -> SimpleExprBuilder Empty Ready ()
    caseBody caseReaders = B.do
        packed <- q <$> arg
        l <- q <$> arg
        returns $ newDo $ B.do
            -- NOTE: 'newDo' Can be removed, but leave it to show it's a do-expression
            tpl <- bind [|runPackedReader reader $packed $l|]
            (tag, packed1, l1) <-
                liftA3
                    (,,)
                    (getField' '(,,) 0 tpl $ \n -> [p|($(q n) :: Tag)|])
                    (getField '(,,) 1 tpl)
                    (getField '(,,) 2 tpl)
            returns $ case_ tag $ B.do
                forM_ ([0 ..] `zip` caseReaders) $ \(i, caseReader) ->
                    matchConst (litP $ IntegerL i) [|runPackedReader $(q caseReader) $(q packed1) $(q l1)|]
                matchWild [|fail "Bad Tag"|]
