{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}

module Data.Packed.NaturalTH.Write (genWrite, genConWrite) where

import Control.Monad
import Data.Packed (Packable)
import qualified Data.Packed as P
import Data.Packed.Needs
import Data.Packed.TH.Utils (getNameAndBangTypesFromCon, resolveAppliedType, sanitizeConName)
import Data.Word (Word8)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Case (body, case_, field, matchCon, var)
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Name
import Language.Haskell.TH.Natural.Syntax.Signature
import Language.Haskell.TH.Quotable

genWrite :: TH.Name -> TH.DecsQ
genWrite tyName = newFunc ("write" ++ TH.nameBase tyName) $ B.do
    inline
    setSignature writeSig
    bodyFromExp writeBody
  where
    writeBody = newExpr $ B.do
        (TH.TyConI (TH.DataD _ _ _ _ cs _)) <- liftB $ TH.reify tyName
        obj <- arg
        returns $
            case_ obj $
                forM_ cs $ \con -> B.do
                    let (conName, conParamTypes) = getNameAndBangTypesFromCon con
                    matchCon conName $ B.do
                        fields <- forM ([0 ..] `zip` conParamTypes) $ \(i, _) -> field i var
                        body $ newExpr $ returns $ apply (TH.VarE $ conWriteFName conName) fields
    writeSig = newSignature $ B.do
        (resolvedType, typeVariables) <- liftB $ resolveAppliedType tyName
        r <- newTypeVar "r"
        t <- newTypeVar "t"
        forM_ typeVariables $ \tyVar ->
            addConstraint [t|P.Packable $(TH.varT tyVar)|]
        addParam resolvedType
        setResultType [t|NeedsWriter $(q resolvedType) $(qEC r) $(qEC t)|]

conWriteFName :: TH.Name -> TH.Name
conWriteFName conName = TH.mkName $ "writeCon" ++ sanitizeConName conName

genConWrite :: TH.Name -> TH.Con -> Integer -> TH.DecsQ
genConWrite tyName con tag = newFunc (TH.nameBase $ conWriteFName conName) $ B.do
    inline
    setSignature conWriteSig
    bodyFromExp conWriteBody
  where
    (conName, conParamTypes) = getNameAndBangTypesFromCon con
    conWriteBody = B.do
        args <- forM conParamTypes $ const arg
        returns $
            foldl
                (\rest conArg -> [|$rest Data.Packed.Needs.>> write $(q conArg)|])
                [|mkNeedsBuilder (\n -> runBuilder (write ($(TH.litE $ TH.IntegerL tag) :: Word8)) (unsafeCastNeeds n))|]
                args
    conWriteSig = newSignature $ B.do
        (resolvedType, typeVariables) <- liftB $ resolveAppliedType tyName
        r <- newTypeVar "r"
        t <- newTypeVar "t"
        forM_ typeVariables $ \tyVar -> B.do
            addConstraint [t|Packable $(TH.varT tyVar)|]
        forM_ conParamTypes $ \(_, argTy) -> B.do
            addParam argTy
        setResultType [t|NeedsWriter $(q resolvedType) $(qEC r) $(qEC t)|]
