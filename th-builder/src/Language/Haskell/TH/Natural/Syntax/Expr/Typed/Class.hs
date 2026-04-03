{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Typed.Class (
    IsTypedExprBuilder (..),
    genExpr,
    addDeconstruct,
    addLet,
    letCount,
    returns,
    strictLetBind,
    letBind,
    letBind_,
) where

import Data.Constructor.Extract.Class
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen (GenTExpr (genTExpr))
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Expr.Internal
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Language.Haskell.TH.Natural.Syntax.Expr.Untyped (Definition, IsExprBuilder, runExprBuilder)
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Untyped as Untyped
import qualified Language.Haskell.TH.Syntax as TH

class IsTypedExprBuilder st where
    runTypedExprBuilder ::
        TypedExprBuilder st '[] args (Returns a) () ->
        TH.Q (TH.TExp (args :~> Returns a))

instance (IsExprBuilder st, Definition st ~ TH.Q a, ExtractedConstructor a TH.Exp) => IsTypedExprBuilder st where
    runTypedExprBuilder (MkTEB b) = TH.TExp . fromEC <$> runExprBuilder b

-- TODO Instance of GenTexp

-- | Alias to 'runTypedExprBuilder'
genExpr :: (IsExprBuilder st, Definition st ~ TH.Q a, ExtractedConstructor a TH.Exp) => TypedExprBuilder st '[] args (Returns a) () -> TH.Q (TH.TExp (args :~> Returns a))
genExpr = runTypedExprBuilder

addDeconstruct :: (IsExprBuilder st) => Deconstruct -> TypedExprBuilder st args args Unknown ()
addDeconstruct = unsafeUntyped . Untyped.addDeconstruct

addLet :: (IsExprBuilder st) => Binding -> TypedExprBuilder st args args Unknown ()
addLet = unsafeUntyped . Untyped.addLet

letCount :: (IsExprBuilder st) => TypedExprBuilder st args args res Int
letCount = unsafeUntyped Untyped.letCount

returns ::
    forall t b args st.
    (IsExprBuilder st) =>
    (GenTExpr t b) =>
    b -> TypedExprBuilder st args args (Returns t) ()
returns b = unsafeUntyped $ B.do
    e <- TH.unType <$> liftB (genTExpr @t b)
    Untyped.returns e

strictLetBind :: (IsExprBuilder st, GenTExpr t b) => b -> TypedExprBuilder st args args Unknown (TH.TExp t)
strictLetBind = letBind_ True

letBind :: (IsExprBuilder st, GenTExpr t b) => b -> TypedExprBuilder st args args Unknown (TH.TExp t)
letBind = letBind_ False

letBind_ :: forall t b st args. (IsExprBuilder st, GenTExpr t b) => Bool -> b -> TypedExprBuilder st args args Unknown (TH.TExp t)
letBind_ isStrict b = MkTEB $ B.do
    e <- TH.unType <$> liftB (genTExpr @t b)
    unsafeCastStep (TH.TExp <$> Untyped.letBind_ isStrict e)
