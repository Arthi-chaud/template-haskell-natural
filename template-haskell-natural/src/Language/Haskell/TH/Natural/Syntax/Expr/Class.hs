{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Class where

import Data.Kind
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Common
import Language.Haskell.TH.Natural.Syntax.Internal.Builder

class ExprBuilder (m :: BuilderStep -> BuilderStep -> Type -> Type) where
    type Definition m
    getField :: (THBuilder b TH.Exp) => TH.Name -> Int -> b -> m step Empty TH.Exp

    addLet :: LetBinding -> m step Empty ()
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
    addLet $ MkLet bindName expr strict
    return $ TH.VarE bindName
