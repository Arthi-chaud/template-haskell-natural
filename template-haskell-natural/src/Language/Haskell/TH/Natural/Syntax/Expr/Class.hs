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
    getTupleField,
    getTupleField',
    getField'',
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

class ExprBuilder (m :: BuilderStep -> BuilderStep -> Type -> Type) where
    type Definition m

    -- withDeconstruct :: TH.Exp -> (Maybe Deconstruct -> m step step (a, Deconstruct)) -> m step Empty a
    addDeconstruct :: Deconstruct -> m step Empty ()

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

getField'' ::
    (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) =>
    -- | The constructor used to deconstruct
    Either Int TH.Name ->
    -- | The index of the field in the constructor
    Int ->
    -- | The expression to deconstruct
    b ->
    -- | Modify the created pattern (e.g. add BangP or type annotation)
    (TH.Pat -> TH.Q TH.Pat) ->
    m step Empty TH.Exp
getField'' conName idx qExpr fPat = unsafeCastStep $ do
    expr <- liftB $ gen qExpr
    patVarName <- liftB $ TH.newName "pat"
    pat <- liftB $ gen $ fPat $ TH.VarP patVarName
    fieldCount <- liftB $ either pure conFieldCount conName
    addDeconstruct $ MkDec conName [(idx, pat)] expr fieldCount
    return $ TH.VarE patVarName

getField :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => TH.Name -> Int -> b -> m step Empty TH.Exp
getField conName idx qExpr = getField'' (Right conName) idx qExpr pure

getField' :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => TH.Name -> Int -> b -> (TH.Pat -> TH.Q TH.Pat) -> m step Empty TH.Exp
getField' conName = getField'' (Right conName)

getTupleField :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => Int -> Int -> b -> m step Empty TH.Exp
getTupleField size idx qExpr = getField'' (Left size) idx qExpr pure

getTupleField' :: (ExprBuilder m, QBuilder b TH.Exp, m ~ Builder s) => Int -> Int -> b -> (TH.Pat -> TH.Q TH.Pat) -> m step Empty TH.Exp
getTupleField' size = getField'' (Left size)

strict :: TH.Pat -> TH.Q TH.Pat
strict = pure . TH.BangP
