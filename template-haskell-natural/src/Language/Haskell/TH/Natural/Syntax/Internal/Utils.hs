module Language.Haskell.TH.Natural.Syntax.Internal.Utils (conFieldCount) where

import Data.Constructor.Extract (ExtractedConstructor (toExtractedCon))
import Data.List (find)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons

conFieldCount :: Name -> Q Int
conFieldCount conName_ = do
    info <- reify conName_
    case toExtractedCon info of
        Just (MkDataConI n' _ pname) -> do
            parentInfo <- reify pname
            case toExtractedCon parentInfo of
                Just (MkTyConI dec) -> return $ countFieldForCon dec n'
                _ -> fail "Expected a type constructor"
        Nothing -> fail "Expected the name to be one of  a data constructor"
  where
    countFieldForCon dec n = case dec of
        DataD _ _ _ _ cons _ -> maybe 0 snd $ find fst $ fmap (countConArg conName_) cons
        NewtypeD _ _ _ _ con _ -> snd $ countConArg n con
        _ -> 0
    countConArg n = \case
        NormalC n' args -> (n == n', length args)
        RecC n' args -> (n == n', length args)
        InfixC _ n' _ -> (n == n', 2)
        ForallC _ _ con -> countConArg n con
        GadtC ns args _ -> (n `elem` ns, length args)
        RecGadtC ns args _ -> (n `elem` ns, length args)
