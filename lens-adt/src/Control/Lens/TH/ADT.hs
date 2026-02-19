module Control.Lens.TH.ADT (
    makeADTLenses,
    makeADTLens,

    -- * Internal
    lensClassName,
) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.Generics.Product
import Data.List ((!?))
import Data.Maybe
import Language.Haskell.TH

-- | Generates lenses for all the fields of the given type.
makeADTLenses :: Name -> [String] -> DecsQ
makeADTLenses tyName fieldNames =
    concat
        <$> zipWithM (makeADTLens tyName) fieldNames [0 ..]

-- | Generates a lens with the given name for the field at the given index for the given type
--
-- @
-- makeADTLens '(,) "left" 0
--
-- -- Generates the following
-- instance HasLeft (a,b) a where
--  left = 'position' @1
-- @
makeADTLens :: Name -> String -> Int -> DecsQ
makeADTLens tyName lensStrName fieldIdx = do
    let lensName = mkName lensStrName
    instance_ <- makeADTLensInstance tyName lensName fieldIdx
    class_ <-
        lookupTypeName (nameBase $ lensClassName lensName) >>= \case
            Nothing -> Just <$> makeADTLensClass lensName
            Just _ -> return Nothing
    return (maybeToList class_ ++ [instance_])

makeADTLensInstance :: Name -> Name -> Int -> DecQ
makeADTLensInstance tyName lensName fieldIdx = do
    (tyArgs, bt) <-
        reify tyName >>= \case
            TyConI (DataD _ _ tyBndrs _ [NormalC _ con] _) ->
                let tyArgs =
                        tyBndrs <&> \case
                            PlainTV n _ -> n
                            KindedTV n _ _ -> n
                 in return (tyArgs, con)
            e -> fail $ "Expected a data type with extractly one constructor, got: " ++ show e
    fieldType <- snd <$> maybe (fail "Invalid field index") return (bt !? fieldIdx)
    let sourceTy = foldl (\rest t -> rest `AppT` VarT t) (ConT tyName) tyArgs
    return $
        InstanceD
            Nothing
            []
            (ConT (lensClassName lensName) `AppT` sourceTy `AppT` fieldType)
            [ FunD
                lensName
                [Clause [] (NormalB (VarE 'position `AppTypeE` LitT (NumTyLit $ fromIntegral fieldIdx + 1))) []]
            ]

makeADTLensClass :: Name -> DecQ
makeADTLensClass lensName =
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

lensClassName :: Name -> Name
lensClassName lensName = mkName $ "Has" ++ capitalize (nameBase lensName)
  where
    capitalize = over _head toUpper
