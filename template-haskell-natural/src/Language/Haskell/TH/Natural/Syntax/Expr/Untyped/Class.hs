{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Untyped.Class (
    IsExprBuilder (..),

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

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Internal.Utils
import Language.Haskell.TH.Natural.Syntax.Builder hiding (fail)
import Language.Haskell.TH.Natural.Syntax.Expr.Internal

class IsExprBuilder st where
    type Definition st

    addDeconstruct :: Deconstruct -> Builder st step Empty ()

    addLet :: Binding -> Builder st step Empty ()
    letCount :: Builder st step step Int

    returns :: (GenExpr b) => b -> Builder st step Ready ()
    runExprBuilder :: Builder st step Ready () -> Definition st

instance (IsExprBuilder st, GenExpr (Definition st)) => GenExpr (Builder st step Ready ()) where
    genExpr = genExpr . runExprBuilder

strictLetBind :: (IsExprBuilder st, GenExpr b) => b -> Builder st step Empty TH.Exp
strictLetBind = letBind_ True

letBind :: (IsExprBuilder st, GenExpr b) => b -> Builder st step Empty TH.Exp
letBind = letBind_ False

letBind_ :: (IsExprBuilder st, GenExpr b) => Bool -> b -> Builder st step Empty TH.Exp
letBind_ isStrict b = unsafeCastStep $ do
    prevLetCount <- letCount
    bindName <- liftB $ TH.newName ("var" ++ show prevLetCount)
    expr <- liftB $ genExpr b
    addLet $ MkBind bindName expr isStrict
    return $ TH.VarE bindName

getField'' ::
    (IsExprBuilder st, GenExpr b) =>
    -- | The constructor used to deconstruct
    Either Int TH.Name ->
    -- | The index of the field in the constructor
    Int ->
    -- | The expression to deconstruct
    b ->
    -- | Modify the created pattern (e.g. add BangP or type annotation)
    (TH.Pat -> TH.Q TH.Pat) ->
    Builder st step Empty TH.Exp
getField'' conName idx qExpr fPat = unsafeCastStep $ do
    expr <- liftB $ genExpr qExpr
    patVarName <- liftB $ TH.newName "pat"
    pat <- liftB $ genPat $ fPat $ TH.VarP patVarName
    fieldCount <- liftB $ either pure conFieldCount conName
    addDeconstruct $ MkDec conName [(idx, pat)] expr fieldCount
    return $ TH.VarE patVarName

getField :: (IsExprBuilder st, GenExpr b) => TH.Name -> Int -> b -> Builder st step Empty TH.Exp
getField conName idx qExpr = getField'' (Right conName) idx qExpr pure

getField' :: (IsExprBuilder st, GenExpr b) => TH.Name -> Int -> b -> (TH.Pat -> TH.Q TH.Pat) -> Builder st step Empty TH.Exp
getField' conName = getField'' (Right conName)

getTupleField :: (IsExprBuilder st, GenExpr b) => Int -> Int -> b -> Builder st step Empty TH.Exp
getTupleField size idx qExpr = getField'' (Left size) idx qExpr pure

getTupleField' :: (IsExprBuilder st, GenExpr b) => Int -> Int -> b -> (TH.Pat -> TH.Q TH.Pat) -> Builder st step Empty TH.Exp
getTupleField' size = getField'' (Left size)

strict :: TH.Pat -> TH.Q TH.Pat
strict = pure . TH.BangP
