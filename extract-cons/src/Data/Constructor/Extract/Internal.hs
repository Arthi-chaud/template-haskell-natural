module Data.Constructor.Extract.Internal (DataAndCon (..), consTypeArgNames, dataAndConFromName) where

import Data.Generics
import Data.Maybe
import Language.Haskell.TH

data DataAndCon = MkDataAndCon
    { dataName :: Name
    , dataTypeArgNames :: [Name]
    , conName :: Name
    , conArgs :: [Type]
    }
    deriving (Eq, Show)

-- | Build 'DataAndCon' using the given 'Dec' (must be a 'DataD') ad the target constructor's name
fromDataD :: (MonadFail m) => Dec -> Name -> m DataAndCon
fromDataD (DataD _ dataName tyVarBnd _ cons _) expectedConName = do
    let dataTypeArgNames = tyVarBndToTyName <$> tyVarBnd
    (conName, conArgs) <-
        maybe
            (fail $ "Could not find constructor with name " ++ nameBase expectedConName)
            return
            $ safeHead
            $ mapMaybe getConNameAndArgs cons
    return MkDataAndCon{..}
  where
    getConNameAndArgs (NormalC n bt)
        | n == expectedConName = Just (n, map snd bt)
    getConNameAndArgs _ = Nothing
    tyVarBndToTyName = \case
        PlainTV t _ -> t
        KindedTV t _ _ -> t
    safeHead = \case
        [] -> Nothing
        (a : _) -> Just a
fromDataD x _ = fail $ "Expected data declaration, got: " ++ show x

-- | Get 'DataAndCon' from the target constructor's name
dataAndConFromName :: Name -> Q DataAndCon
dataAndConFromName name = do
    info <- reify name
    case info of
        DataConI n t p ->
            reify p >>= \case
                TyConI d@(DataD{}) -> fromDataD d name
                x -> fail ("Expected data definition, got: " ++ show x)
        x -> fail ("Expected a data constructor, got: " ++ show x)

consTypeArgNames :: DataAndCon -> [Name]
consTypeArgNames dc = everything (++) ([] `mkQ` getVarName) $ conArgs dc
  where
    getVarName = \case
        VarT n -> [n]
        _ -> []
