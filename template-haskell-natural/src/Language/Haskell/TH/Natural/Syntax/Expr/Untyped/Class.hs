{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Untyped.Class (
    IsExprBuilder (..),

    -- * Let
    letBind,
    strictLetBind,
    letBind_,

    -- * Deconstruction
    getField,
    getTupleField,
    getFields,
    getTupleFields,
    getField',
    getField_,
    getTupleField',
    getField'',
    strict,
) where

import Control.Monad
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Internal.Utils
import Language.Haskell.TH.Natural.Syntax.Builder hiding (fail)
import Language.Haskell.TH.Natural.Syntax.Expr.Internal

-- | Typeclass to factorise the common behaviour of expression builders
class IsExprBuilder st where
    type Definition st

    addDeconstruct :: Deconstruct -> Builder st step Empty ()

    addLet :: Binding -> Builder st step Empty ()
    letCount :: Builder st step step Int

    returns :: (GenExpr b) => b -> Builder st step Ready ()
    runExprBuilder :: Builder st step Ready () -> Definition st

instance (IsExprBuilder st, GenExpr (Definition st)) => GenExpr (Builder st step Ready ()) where
    genExpr = genExpr . runExprBuilder

-- | Let-Bind an expression
letBind :: (IsExprBuilder st, GenExpr b) => b -> Builder st step Empty TH.Exp
letBind = letBind_ False

-- | Let-Bind an expression with a strict pattern
strictLetBind :: (IsExprBuilder st, GenExpr b) => b -> Builder st step Empty TH.Exp
strictLetBind = letBind_ True

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
    -- | The number of fields in the constructor, if known
    -- | The expression to deconstruct
    Maybe Int ->
    b ->
    -- | Modify the created pattern (e.g. add BangP or type annotation)
    (TH.Pat -> TH.Q TH.Pat) ->
    Builder st step Empty TH.Exp
getField'' cName idx fCount qExpr fPat = unsafeCastStep $ do
    expr <- liftB $ genExpr qExpr
    patVarName <- liftB $ TH.newName "pat"
    pat <- liftB $ genPat $ fPat $ TH.VarP patVarName
    fieldCount <- liftB $ maybe (either pure conFieldCount cName) pure fCount
    addDeconstruct $ MkDec cName [(idx, pat)] expr fieldCount
    return $ TH.VarE patVarName

-- | Deconstruct a value and get the nth field of the constructor
getField ::
    (IsExprBuilder st, GenExpr b) =>
    -- | Constructor name
    TH.Name ->
    -- | index of the field
    Int ->
    -- | Expr to deconstruct
    b ->
    Builder st step Empty TH.Exp
getField cName idx qExpr = getField'' (Right cName) idx Nothing qExpr pure

-- | Same as 'getField', but allow customising the bound pattern
getField' :: (IsExprBuilder st, GenExpr b) => TH.Name -> Int -> b -> (TH.Pat -> TH.Q TH.Pat) -> Builder st step Empty TH.Exp
getField' cName idx = getField'' (Right cName) idx Nothing

-- | Similar to 'getField'. Useful when the constructor to use isn't accessible through 'reify'
getField_ ::
    (IsExprBuilder st, GenExpr b) =>
    TH.Name ->
    -- | The index of the field to get
    Int ->
    -- | The number of fields in the constructor
    Int ->
    b ->
    Builder st step Empty TH.Exp
getField_ cName idx fCount b = getField'' (Right cName) idx (Just fCount) b pure

-- | Like 'getField', but for tuples
getTupleField ::
    (IsExprBuilder st, GenExpr b) =>
    -- | Tuple size
    Int ->
    -- | Field Index
    Int ->
    b ->
    Builder st step Empty TH.Exp
getTupleField size idx qExpr = getField'' (Left size) idx (Just size) qExpr pure

-- | Like 'getField\'', but for tuples
getTupleField' ::
    (IsExprBuilder st, GenExpr b) =>
    -- | Tuple size
    Int ->
    -- | Field Index
    Int ->
    b ->
    (TH.Pat -> TH.Q TH.Pat) ->
    Builder st step Empty TH.Exp
getTupleField' size idx = getField'' (Left size) idx Nothing

-- | Deconstruct and get all the fields in the constructor
getFields ::
    (IsExprBuilder st, GenExpr b) =>
    TH.Name ->
    -- | Field count
    Int ->
    b ->
    Builder st step Empty [TH.Exp]
getFields n fcount b = unsafeCastStep $ do
    e <- liftB $ genExpr b
    forM [0 .. fcount - 1] $ \i -> getField_ n i fcount e

-- | Same as 'getFields', but for tuples
getTupleFields ::
    (IsExprBuilder st, GenExpr b) =>
    -- | Tuple size
    Int ->
    b ->
    Builder st step Empty [TH.Exp]
getTupleFields size b = unsafeCastStep $ do
    e <- liftB $ genExpr b
    forM [0 .. size - 1] $ \i -> getTupleField size i e

-- | Util for 'getField\'' to set the pattern as strict
strict :: TH.Pat -> TH.Q TH.Pat
strict = pure . TH.BangP
