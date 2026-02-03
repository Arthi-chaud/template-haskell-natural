{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Class (
    ExprBuilder (..),

    -- * Let
    strictLetBind,
    letBind,
    letBind_,

    -- * Deconstruction
    getField,
) where

import Control.Monad
import Data.Kind
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Common
import Language.Haskell.TH.Natural.Syntax.Internal.Builder
import Language.Haskell.TH.Natural.Syntax.Internal.Utils
import Text.Printf

class ExprBuilder (m :: BuilderStep -> BuilderStep -> Type -> Type) where
    type Definition m

    withDeconstruct :: TH.Exp -> (Maybe Deconstruct -> m step step (a, Deconstruct)) -> m step Empty a

    addLet :: Binding -> m step Empty ()
    letCount :: m step step Int

    returns :: (THBuilder b TH.Exp) => b -> m step Ready ()
    runExprBuilder :: m step Ready () -> Definition m

strictLetBind :: (ExprBuilder m, THBuilder b TH.Exp, m ~ Builder s) => b -> m step Empty TH.Exp
strictLetBind = letBind_ True

letBind :: (ExprBuilder m, THBuilder b TH.Exp, m ~ Builder s) => b -> m step Empty TH.Exp
letBind = letBind_ False

letBind_ :: (ExprBuilder m, THBuilder b TH.Exp, m ~ Builder s) => Bool -> b -> m step Empty TH.Exp
letBind_ strict b = unsafeCastStep $ do
    prevLetCount <- letCount
    bindName <- liftB $ TH.newName ("var" ++ show prevLetCount)
    expr <- liftB $ gen b
    addLet $ MkBind bindName expr strict
    return $ TH.VarE bindName

getField :: (ExprBuilder m, THBuilder b TH.Exp, m ~ Builder s) => TH.Name -> Int -> b -> m step Empty TH.Exp
getField conName idx qExpr = unsafeCastStep $ do
    expr <- liftB $ gen qExpr
    withDeconstruct expr $ \case
        Nothing -> do
            patVarName <- liftB $ TH.newName "pat"
            fieldCount <- liftB $ conFieldCount conName
            let newDecons = MkDec conName [(idx, patVarName)] expr fieldCount
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
            let newDecons = MkDec conName ((idx, patVarName) : fieldVarNames) expr totalFieldCount
            return (TH.VarE patVarName, newDecons)

-- TODO Strict getFields
