{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typeclasses to standardise the arguments that the various functions accept
module Language.Haskell.TH.Gen (
    GenDec (..),
    GenDecs (..),
    GenExpr (..),
    GenTExpr (..),
    GenType (..),
    GenPat (..),
) where

import Data.Constructor.Extract.Class
import Data.List (singleton)
import Language.Haskell.TH

class GenDec a where
    genDec :: a -> Q Dec

instance GenDec Dec where
    genDec = pure

instance (GenDec a) => GenDec (Q a) where
    genDec qb = qb >>= genDec

instance (ExtractedConstructor a Dec) => GenDec a where
    genDec = pure . fromEC

class GenDecs a where
    genDecs :: a -> Q [Dec]

instance GenDecs [Dec] where
    genDecs = pure

instance (GenDecs a) => GenDecs (Q a) where
    genDecs qb = qb >>= genDecs

instance (GenDec a) => GenDecs a where
    genDecs = fmap singleton . genDec

class GenExpr a where
    genExpr :: a -> Q Exp

instance GenExpr Exp where
    genExpr = pure

instance (m ~ Q) => GenExpr (m Exp) where
    genExpr = id

instance (ExtractedConstructor a Exp) => GenExpr a where
    genExpr = pure . fromEC

instance (GenExpr a) => GenExpr (Q a) where
    genExpr qb = qb >>= genExpr

class GenTExpr t a where
    genTExpr :: a -> Q (TExp t)

instance (m ~ Q) => GenTExpr t (Code m t) where
    genTExpr = examineCode

class GenType a where
    genTy :: a -> Q Type

instance GenType Type where
    genTy = pure

instance (ExtractedConstructor a Type) => GenType a where
    genTy = pure . fromEC

instance GenType (Q Type) where
    genTy = id

instance (m ~ Q, GenType a) => GenType (m a) where
    genTy qb = qb >>= genTy

class GenPat a where
    genPat :: a -> Q Pat

instance GenPat (Q Pat) where
    genPat = id

instance GenPat Pat where
    genPat = pure

instance (ExtractedConstructor a Pat) => GenPat a where
    genPat = pure . fromEC
