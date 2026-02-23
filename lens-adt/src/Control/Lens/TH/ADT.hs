-- | This module allows generating lenses for non-record ADTs.
--
-- To allow overloading (e.g. using the same lens name for multiple ADTs), it first generates a typeclass and then an instance for it.
-- If other lenses generated in the same scope have the same name, only an instance is generated.
--
-- @
-- data A = A Int
-- data B = B Int
--
-- 'makeADTLenses' 'A ["value"]
--
-- -- ===> Generates the following
-- class HasValue a b where
--  value = 'Lens\'' a b
--
-- instance HasValue A Int where
--  value = 'position' @1
--
-- 'makeADTLenses' 'B ["value"]
--
-- -- ===> Generates the following
--
-- instance HasValue B Int where
--  value = 'position' @1
-- @
module Control.Lens.TH.ADT (
    makeADTLenses,
    makeADTLens,
    lensClassName,
) where

import Control.Lens
import Control.Monad
import Data.Char
import Data.Generics.Product
import Data.List ((!?))
import Data.Maybe
import Language.Haskell.TH

-- | Generates lenses for all the fields of the given type. Uses 'makeADTLens' for each field.
makeADTLenses :: Name -> [String] -> DecsQ
makeADTLenses tyName fieldNames =
    concat
        <$> zipWithM (makeADTLens tyName) fieldNames [0 ..]

-- | Generates a lens with the given name for the field at the given index for the given type
--
-- @
-- 'makeADTLens' '(,) "left" 0
--
-- -- ===> Generates the following
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

-- | Get the name of the lens typeclass to define, using the lens' name
lensClassName :: Name -> Name
lensClassName lensName = mkName $ "Has" ++ capitalize (nameBase lensName)
  where
    capitalize = over _head toUpper
