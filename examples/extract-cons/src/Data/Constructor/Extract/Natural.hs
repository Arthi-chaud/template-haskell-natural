{-# LANGUAGE QualifiedDo #-}

module Data.Constructor.Extract.Natural (extractConstructor) where

import Control.Monad
import Data.Constructor.Extract (ExtractOptions (..), dataConstructorName, dataDeclarationName)
import Data.Constructor.Extract.Class
import Data.Constructor.Extract.Internal
import Language.Haskell.TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Case
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Natural.Syntax.Datatype.Con.Normal (addField', newCon)
import Language.Haskell.TH.Natural.Syntax.Datatype.Data
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Instance
import Language.Haskell.TH.Quotable
import Language.Haskell.TH.Syntax.ExtractedCons hiding (arg, body)

-- | Using a constructor's 'Name', generates a new data type with only this constructor.
extractConstructor :: Name -> ExtractOptions -> DecsQ
extractConstructor n opts = do
    dataAndCon <- dataAndConFromName n
    dataDec <- generateDataDeclaration dataAndCon opts
    instanceDec <- generateExtractedConInstance dataAndCon opts
    return [dataDec, instanceDec]

generateDataDeclaration :: DataAndCon -> ExtractOptions -> DecQ
generateDataDeclaration d@MkDataAndCon{..} opts = genDec $ newData (nameBase $ dataDeclarationName d opts) $ B.do
    forM_ (conTypeArgNames d) (addTypeVar . MkVarT)
    forM_ (deriveClasses opts) addDeriving
    addCon $ newCon (nameBase $ dataConstructorName d opts) $ B.do
        forM_ conArgs addField'

generateExtractedConInstance :: DataAndCon -> ExtractOptions -> DecQ
generateExtractedConInstance d@MkDataAndCon{..} opts = genDec $ newInstance ''ExtractedConstructor $ B.do
    addInstanceArg (applyT (ConT $ dataDeclarationName d opts) (VarT <$> conTypeArgNames d))
    addInstanceArg (applyT (ConT dataName) (VarT <$> dataTypeArgNames))
    let conArgsCount = length conArgs
        genConName = dataConstructorName d opts
    addBody' $ newFunc "fromEC" $ bodyFromExp $ B.do
        ec <- arg
        ecFields <- getFields genConName conArgsCount ec
        returns $ apply (ConE conName) ecFields

    addBody' $ newFunc "toEC" $ bodyFromExp $ B.do
        baseData <- arg
        returns $ case_ baseData $ B.do
            matchCon conName $ B.do
                fs <- forM [0 .. conArgsCount - 1] $ \i -> field i var
                body [|Just $(q $ apply (ConE genConName) fs)|]
            matchWild [|Nothing|]
