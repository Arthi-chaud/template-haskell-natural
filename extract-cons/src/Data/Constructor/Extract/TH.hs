module Data.Constructor.Extract.TH (extractConstructor, extractConstructorsOf) where

import Data.Constructor.Extract.Class
import Data.Constructor.Extract.Internal
import Data.Constructor.Extract.Options
import Language.Haskell.TH

-- | Using a constructor's 'Name', generates a new data type with only this constructor.
extractConstructor :: Name -> ExtractOptions -> DecsQ
extractConstructor name opts = do
    dataAndCon <- dataAndConFromName name
    return
        [ generateDataDeclaration dataAndCon opts
        , generateExtractedConInstance dataAndCon opts
        ]

-- | Calls 'extractConstructor' for each constructor of the data type whone 'Name' is passed as parameter.
extractConstructorsOf :: Name -> ExtractOptions -> DecsQ
extractConstructorsOf rawDataName opts =
    conNamesFromTypeName rawDataName
        >>= (fmap concat . mapM (`extractConstructor` opts))

generateDataDeclaration :: DataAndCon -> ExtractOptions -> Dec
generateDataDeclaration d@MkDataAndCon{..} opts@MkExtractOptions{..} =
    DataD [] dataDecName typeBnd Nothing [con] [DerivClause Nothing (ConT <$> deriveClasses)]
  where
    con = NormalC (dataConstructorName d opts) conArgs
    dataDecName = dataDeclarationName d opts
    typeBnd = (`PlainTV` BndrReq) <$> conTypeArgNames d

dataDeclarationName :: DataAndCon -> ExtractOptions -> Name
dataDeclarationName MkDataAndCon{..} MkExtractOptions{..} = mkName $ newDataName (nameBase conName)

dataConstructorName :: DataAndCon -> ExtractOptions -> Name
dataConstructorName MkDataAndCon{..} MkExtractOptions{..} = mkName $ newConName (nameBase conName)

generateExtractedConInstance :: DataAndCon -> ExtractOptions -> Dec
generateExtractedConInstance d@MkDataAndCon{..} opts =
    InstanceD Nothing [] instanceType [fromDec, toDec]
  where
    instanceType = ConT ''ExtractedConstructor `AppT` instanceFromTypeArg `AppT` instanceToTypeArg
    instanceFromTypeArg = applyTypeVar (ConT $ dataDeclarationName d opts) (conTypeArgNames d)
    instanceToTypeArg = applyTypeVar (ConT dataName) dataTypeArgNames
    applyTypeVar = foldl (\rest tyArg -> rest `AppT` VarT tyArg)
    --
    genConstructorName = dataConstructorName d opts
    fieldNames = zipWith (\i _ -> mkName . ("f" ++) $ show i) [(0 :: Int) ..] conArgs
    applyArgs = foldl (\rest field -> rest `AppE` VarE field)
    conArgPatterns = VarP <$> fieldNames
    --
    fromDec =
        let
            body = NormalB $ applyArgs (ConE conName) fieldNames
         in
            FunD 'fromEC [Clause [ConP (dataConstructorName d opts) [] conArgPatterns] body []]
    --
    toDec =
        let
            body = NormalB $ ConE 'Just `AppE` applyArgs (ConE genConstructorName) fieldNames
         in
            FunD
                'toEC
                [ Clause [ConP conName [] conArgPatterns] body []
                , Clause [WildP] (NormalB $ ConE 'Nothing) []
                ]
