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

import Data.Constructor.Extract.Class (ExtractedConstructor (fromEC))
import Language.Haskell.TH (TExp (unType))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Expr.Class (Definition, IsExprBuilder, runExprBuilder)
import qualified Language.Haskell.TH.Natural.Syntax.Expr.Class as Untyped
import Language.Haskell.TH.Natural.Syntax.Expr.Internal
import Language.Haskell.TH.Natural.Syntax.Expr.Typed.Builder
import Language.Haskell.TH.QBuilder
import qualified Language.Haskell.TH.Syntax as TH

class IsTypedExprBuilder st where
    runTypedExprBuilder ::
        TypedExprBuilder st '[] args (Returns a) () ->
        TH.Q (TH.TExp (ExprType args (Returns a)))

instance (IsExprBuilder st, Definition st ~ TH.Q a, ExtractedConstructor a TH.Exp) => IsTypedExprBuilder st where
    runTypedExprBuilder (MkTEB b) = TH.TExp . fromEC <$> runExprBuilder b

-- | Alias to 'runTypedExprBuilder'
genExpr :: (IsExprBuilder st, Definition st ~ TH.Q a, ExtractedConstructor a TH.Exp) => TypedExprBuilder st '[] args (Returns a) () -> TH.Q (TExp (ExprType args (Returns a)))
genExpr = runTypedExprBuilder

addDeconstruct :: (IsExprBuilder st) => Deconstruct -> TypedExprBuilder st args args Unknown ()
addDeconstruct = unsafeUntyped . Untyped.addDeconstruct

addLet :: (IsExprBuilder st) => Binding -> TypedExprBuilder st args args Unknown ()
addLet = unsafeUntyped . Untyped.addLet

letCount :: (IsExprBuilder st) => TypedExprBuilder st args args res Int
letCount = unsafeUntyped Untyped.letCount

returns ::
    forall a b args st.
    (IsExprBuilder st) =>
    (QBuilder b (TH.TExp a)) =>
    b -> TypedExprBuilder st args args (Returns a) ()
returns b = unsafeUntyped $ B.do
    e <- unType <$> liftB (gen @b @(TH.TExp a) b)
    Untyped.returns e

strictLetBind :: (IsExprBuilder st, QBuilder b (TH.TExp a)) => b -> TypedExprBuilder st args args Unknown (TH.TExp a)
strictLetBind = letBind_ True

letBind :: (IsExprBuilder st, QBuilder b (TH.TExp a)) => b -> TypedExprBuilder st args args Unknown (TH.TExp a)
letBind = letBind_ False

letBind_ :: forall a b st args. (IsExprBuilder st, QBuilder b (TH.TExp a)) => Bool -> b -> TypedExprBuilder st args args Unknown (TH.TExp a)
letBind_ isStrict b = MkTEB $ B.do
    e <- unType <$> liftB (gen @b @(TH.TExp a) b)
    unsafeCastStep (TH.TExp <$> Untyped.letBind_ isStrict e)
