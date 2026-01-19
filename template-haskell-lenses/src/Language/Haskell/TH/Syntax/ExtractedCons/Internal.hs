{-# LANGUAGE TypeApplications #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Internal (mkExtractedConsLens) where

import Control.Lens
import Data.Generics.Product.Positions
import Data.List ((!?))
import Language.Haskell.TH

mkExtractedConsLens :: Name -> String -> Int -> DecsQ
mkExtractedConsLens extractedConName lensStrName fieldIdx = do
    let lensName = mkName lensStrName
    signature <- mkExtractedConsLensSignature extractedConName lensName fieldIdx
    body <- funD lensName [clause [] (normalB [|position @($(litT $ numTyLit (1 + fromIntegral fieldIdx)))|]) []]
    return [signature, body]

mkExtractedConsLensSignature :: Name -> Name -> Int -> DecQ
mkExtractedConsLensSignature extractedConName lensName fieldIdx = do
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
    let source = foldl (\rest t -> rest `AppT` VarT t) (ConT extractedConName) tyArgs
    return $ SigD lensName (ConT ''Lens' `AppT` source `AppT` fieldType)
