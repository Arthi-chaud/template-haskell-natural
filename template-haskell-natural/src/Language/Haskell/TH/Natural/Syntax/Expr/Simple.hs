{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Simple (
    -- * Builder
    newExpr,
    SimpleExprDefinition,
    SimpleExprBuilder,

    -- * State
    SimpleExprBuilderState (..),

    -- * Operations
    arg,

    -- * Reexport
    module Language.Haskell.TH.Natural.Syntax.Expr.Class,
) where

import Control.Lens (makeLenses, view, views, (.=), (?=), (^.), (|>=))
import Control.Monad
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder hiding ((>>=))
import Language.Haskell.TH.Natural.Syntax.Expr.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Internal
import Language.Haskell.TH.QBuilder (gen)
import Prelude hiding ((>>=))

type SimpleExprDefinition = TH.Q TH.Exp

data SimpleExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _lets :: [Binding]
    , _deconstructs :: [Deconstruct]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''SimpleExprBuilderState

type SimpleExprBuilder = Builder SimpleExprBuilderState

newExpr :: SimpleExprBuilder step Ready () -> SimpleExprDefinition
newExpr = runExprBuilder

arg :: SimpleExprBuilder curr curr TH.Exp
arg = do
    nextArgName <- liftB $ TH.newName "arg"
    argNames |>= nextArgName
    return $ TH.VarE nextArgName

instance ExprBuilder SimpleExprBuilder where
    type Definition SimpleExprBuilder = SimpleExprDefinition
    returns q = unsafeCastStep $ do
        expr <- liftB $ gen q
        returnedExp ?= expr

    withDeconstruct expr f = impure $ do
        decons <- view deconstructs
        case partition ((== expr) . _src) decons of
            (_, []) -> do
                (varName, decon) <- f Nothing
                deconstructs |>= decon
                return varName
            (prev, decon : rest) -> do
                (varName, decon') <- f $ Just decon
                deconstructs .= prev ++ [decon'] ++ rest
                return varName

    addLet l = impure $ lets |>= l

    letCount = views lets length

    runExprBuilder b = do
        st <- runBaseBuilder b (MkEBS [] [] [] Nothing)
        let binds = st ^. lets <&> bindingToDec
            decons = st ^. deconstructs <&> deconstructToDec
        let lamOrId = case st ^. argNames of
                [] -> id
                names -> TH.LamE (TH.VarP <$> names)
        let letOrId = case binds ++ decons of
                [] -> id
                exprs -> TH.LetE exprs
        resExp <- case st ^. returnedExp of
            Nothing -> fail "Missing returned expression"
            Just e -> return e
        return $ lamOrId $ letOrId resExp
