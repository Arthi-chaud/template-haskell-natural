{-# LANGUAGE TypeApplications #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Internal (mkExtractedConsLens) where

import Control.Lens
import Data.Char
import Data.Generics.Product.Positions
import Data.List ((!?))
import Data.Maybe (maybeToList)
import Language.Haskell.TH

mkExtractedConsLens :: Name -> String -> Int -> DecsQ
mkExtractedConsLens extractedConName lensStrName fieldIdx = do
    let lensName = mkName lensStrName
    class_ <-
        lookupTypeName (nameBase $ lensClassName lensName) >>= \case
            Nothing -> Just <$> mkExtractedConsLensClass lensName
            Just _ -> return Nothing
    instance_ <- mkExtractedConsLensInstance extractedConName lensName fieldIdx
    return (maybeToList class_ ++ [instance_])

lensClassName :: Name -> Name
lensClassName lensName = mkName $ "Has" ++ capitalize (nameBase lensName)
  where
    capitalize = over _head toUpper

mkExtractedConsLensInstance :: Name -> Name -> Int -> DecQ
mkExtractedConsLensInstance extractedConName lensName fieldIdx = do
    (tyArgs, bt) <-
        reify extractedConName >>= \case
            TyConI (DataD _ _ tyBndrs _ [NormalC _ con] _) ->
                let tyArgs =
                        tyBndrs <&> \case
                            PlainTV n _ -> n
                            KindedTV n _ _ -> n
                 in return (tyArgs, con)
            e -> fail $ "Expected a data type with extractly one constructor, got: " ++ show e
    fieldType <- snd <$> maybe (fail "Invalid field index") return (bt !? fieldIdx)
    let sourceTy = foldl (\rest t -> rest `AppT` VarT t) (ConT extractedConName) tyArgs
    return $
        InstanceD
            Nothing
            []
            (ConT (lensClassName lensName) `AppT` sourceTy `AppT` fieldType)
            [ FunD
                lensName
                [Clause [] (NormalB (VarE 'position `AppTypeE` LitT (NumTyLit $ fromIntegral fieldIdx + 1))) []]
            ]

mkExtractedConsLensClass :: Name -> DecQ
mkExtractedConsLensClass lensName = do
    return $
        ClassD
            []
            (lensClassName lensName)
            [PlainTV (mkName "a") BndrReq, PlainTV (mkName "b") BndrReq]
            []
            [SigD lensName (ConT ''Lens' `AppT` VarT (mkName "a") `AppT` VarT (mkName "b"))]
