{-# LANGUAGE TypeApplications #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Internal (mkExtractedConsLenses, mkExtractedConsLens) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.Generics.Product.Positions
import Data.List ((!?))
import Data.Maybe (maybeToList)
import Language.Haskell.TH

mkExtractedConsLenses :: Name -> [String] -> DecsQ
mkExtractedConsLenses extractedConName fieldNames = concat <$> zipWithM (mkExtractedConsLens extractedConName) fieldNames [0 ..]

mkExtractedConsLens :: Name -> String -> Int -> DecsQ
mkExtractedConsLens extractedConName lensStrName fieldIdx = do
    let lensName = mkName lensStrName
    class_ <-
        lookupTypeName (nameBase $ lensClassName lensName) >>= \case
            Nothing -> Just <$> mkExtractedConsLensClass lensName
            Just _ -> return Nothing
    instances_ <- mkExtractedConsLensInstance extractedConName lensName fieldIdx
    return (maybeToList class_ ++ instances_)

lensClassName :: Name -> Name
lensClassName lensName = mkName $ "Has" ++ capitalize (nameBase lensName)
  where
    capitalize = over _head toUpper

mkExtractedConsLensInstance :: Name -> Name -> Int -> DecsQ
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
        baseInstance =
            InstanceD
                Nothing
                []
                (ConT (lensClassName lensName) `AppT` sourceTy `AppT` fieldType)
                [ FunD
                    lensName
                    [Clause [] (NormalB (VarE 'position `AppTypeE` LitT (NumTyLit $ fromIntegral fieldIdx + 1))) []]
                ]

    -- constInstance <-
    --     instanceD
    --         (return [])
    --         (return (ConT (lensClassName lensName) `AppT` (ConT ''Const `AppT` sourceTy `AppT` VarT (mkName "a")) `AppT` fieldType))
    --         [ funD
    --             lensName
    --             [clause [] (normalB [|lens ((^. $(varE lensName)) . getConst) (\(Const st) x -> Const (set $(varE lensName) x st))|]) []]
    --         ]
    return [baseInstance]

mkExtractedConsLensClass :: Name -> DecQ
mkExtractedConsLensClass lensName =
    return $
        ClassD
            []
            (lensClassName lensName)
            [PlainTV a BndrReq, PlainTV b BndrReq]
            [FunDep [a] [b]]
            [SigD lensName (ConT ''Lens' `AppT` VarT a `AppT` VarT b)]
  where
    a = mkName "a"
    b = mkName "b"
