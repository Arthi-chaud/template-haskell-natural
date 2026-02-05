{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Class (
    ExprBuilder (..),

    -- * Let
    strictLetBind,
    letBind,
    letBind_,

    -- * Deconstruction
    getField,
    getField',
    strict,
) where

import Control.Monad
import Data.Kind
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Internal.Utils
import Language.Haskell.TH.Natural.Syntax.Builder hiding (fail)
import Language.Haskell.TH.Natural.Syntax.Expr.Internal
import Language.Haskell.TH.QBuilder
import Text.Printf

class ExprBuilder (m :: BuilderStep -> BuilderStep -> Type -> Type) where
    type Definition m

    withDeconstruct :: TH.Exp -> (Maybe Deconstruct -> m step step (a, Deconstruct)) -> m step Empty a

    addLet :: Binding -> m step Empty ()
    letCount :: m step step Int

    returns :: (QBuilder b TH.Exp) => b -> m step Ready ()
    runExprBuilder :: m step Ready () -> Definition m

instance (ExprBuilder m, QBuilder (Definition m) TH.Exp) => QBuilder (m step Ready ()) TH.Exp where
    gen = gen . runExprBuilder

strictLetBind :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => b -> m step Empty TH.Exp
strictLetBind = letBind_ True

letBind :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => b -> m step Empty TH.Exp
letBind = letBind_ False

letBind_ :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => Bool -> b -> m step Empty TH.Exp
letBind_ isStrict b = unsafeCastStep $ do
    prevLetCount <- letCount
    bindName <- liftB $ TH.newName ("var" ++ show prevLetCount)
    expr <- liftB $ gen b
    addLet $ MkBind bindName expr isStrict
    return $ TH.VarE bindName

getField' :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => TH.Name -> Int -> b -> (TH.Pat -> TH.Q TH.Pat) -> m step Empty TH.Exp
getField' conName idx qExpr fPat = unsafeCastStep $ do
    expr <- liftB $ gen qExpr
    withDeconstruct expr $ \case
        Nothing -> do
            patVarName <- liftB $ TH.newName "pat"
            pat <- liftB $ gen $ fPat $ TH.VarP patVarName
            fieldCount <- liftB $ conFieldCount conName
            let newDecons = MkDec conName [(idx, pat)] expr fieldCount
            return (TH.VarE patVarName, newDecons)
        Just (MkDec conN fieldVarNames _ totalFieldCount) -> do
            when (conN /= conName) $
                fail $
                    printf "The following expression has already been deconstructed with the %s constructor: %s" (show conN) (show expr)
            when (isJust $ lookup idx fieldVarNames) $
                fail $
                    printf "When deconstructing the following expression, the field at index %d in constructor %s has already been bound: %s" idx (show conName) (show expr)
            when (idx >= totalFieldCount) $
                fail $
                    printf "When deconstructing the following expression, the constructor %s has %d fields. Index %d is out of bounds." (show conName) totalFieldCount idx
            patVarName <- liftB $ TH.newName "pat"
            pat <- liftB $ gen $ fPat $ TH.VarP patVarName
            let newDecons = MkDec conName ((idx, pat) : fieldVarNames) expr totalFieldCount
            return (TH.VarE patVarName, newDecons)

getField :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => TH.Name -> Int -> b -> m step Empty TH.Exp
getField conName idx qExpr = getField' conName idx qExpr pure

strict :: TH.Pat -> TH.Q TH.Pat
strict = pure . TH.BangP
