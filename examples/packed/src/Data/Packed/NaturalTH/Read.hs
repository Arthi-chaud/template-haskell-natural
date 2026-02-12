{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}

module Data.Packed.NaturalTH.Read (genRead) where

import Control.Monad
import qualified Data.Packed as R
import Data.Packed.NaturalTH.Case (caseFName)
import qualified Data.Packed.Reader as R
import Data.Packed.TH.Utils
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Signature
import Language.Haskell.TH.QBuilder
import Language.Haskell.TH.Quotable

genRead :: TH.Name -> TH.DecsQ
genRead tyName = newFunc ("read" ++ TH.nameBase tyName) $ B.do
    inline
    setSignature readSig
    bodyFromExp $ newExpr $ B.do
        (TH.TyConI (TH.DataD _ _ _ _ cs _)) <- liftB $ TH.reify tyName
        returns $ apply (TH.VarE $ caseFName tyName) (genReadLambda <$> cs)
  where
    readSig = newSignature $ B.do
        (resolvedType, typeVariables) <- liftB $ resolveAppliedType tyName
        r <- liftB $ newTypeVar "r"
        forM_ typeVariables $ \tyVar ->
            addConstraint [t|R.Unpackable $(TH.varT tyVar)|]
        setResultType [t|R.PackedReader '[$(q resolvedType)] $(qCon r) $(q resolvedType)|]

genReadLambda :: TH.Con -> TH.Q TH.Exp
genReadLambda con = B.do
    let (conName, conParamTypes) = getNameAndBangTypesFromCon con
        go :: [TH.BangType] -> [TH.Exp] -> TH.Q TH.Exp
        go (_ : args) acc =
            [|
                R.reader
                    R.>>= $( gen $ B.do
                                parsedArg <- arg
                                returns (go args (acc ++ [parsedArg]))
                           )
                |]
        go [] conArgs = [|R.return $(apply (TH.ConE conName) conArgs)|]
    go conParamTypes []
