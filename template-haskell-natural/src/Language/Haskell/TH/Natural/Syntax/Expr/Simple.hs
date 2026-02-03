{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Simple (
    -- * Types
    SimpleExprDefinition,

    -- * Builder
    ExprBuilder,
    runExprBuilder,

    -- * Operations
    arg,
    -- returns,
    -- getField,
    -- let_,

    -- * State
    SimpleExprBuilderState (..),
    LetBinding (..),
    Deconstruct (..),
) where

import Control.Lens (makeLenses, view, views, (.=), (?=), (^.), (|>=))
import Control.Monad
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe
import Language.Haskell.TH (mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (gen)
import Language.Haskell.TH.Natural.Syntax.Expr.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Common
import Language.Haskell.TH.Natural.Syntax.Internal hiding ((>>=))
import Language.Haskell.TH.Natural.Syntax.Internal.Utils (conFieldCount)
import Text.Printf
import Prelude hiding ((>>=))

type SimpleExprDefinition = TH.Q TH.Exp

data SimpleExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _lets :: [LetBinding]
    , _deconstructs :: [Deconstruct]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''SimpleExprBuilderState

type SimpleExprBuilder = Builder SimpleExprBuilderState

arg :: SimpleExprBuilder curr curr TH.Exp
arg = do
    prevArgCount <- views argNames length
    let nextArgName = mkName $ 'a' : show (prevArgCount + 1)
    argNames |>= nextArgName
    return $ TH.VarE nextArgName

-- TODO Strict bind + fields

instance ExprBuilder SimpleExprBuilder where
    type Definition SimpleExprBuilder = SimpleExprDefinition
    returns q = unsafeCastStep $ do
        expr <- liftB $ gen q
        returnedExp ?= expr

    addLet l = impure $ lets |>= l

    letCount = views lets length

    getField conName idx qExpr = unsafeCastStep $ do
        expr <- liftB $ gen qExpr
        decons <- view deconstructs
        case partition ((== expr) . _src) decons of
            (_, []) -> do
                patVarName <- liftB $ TH.newName "pat"
                fieldCount <- liftB $ conFieldCount conName
                let newDecons = MkDec conName [(idx, patVarName)] expr fieldCount
                deconstructs |>= newDecons
                return $ TH.VarE patVarName
            (decons', (MkDec conN fieldVarNames _ totalFieldCount) : decons'') -> do
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
                deconstructs .= (decons' ++ [newDecons] ++ decons'')
                return $ TH.VarE patVarName

    runExprBuilder b = do
        st <- runBaseBuilder b (MkEBS [] [] [] Nothing)
        let lamOrId = case st ^. argNames of
                [] -> id
                names -> TH.LamE (TH.VarP <$> names)
        resExp <- case st ^. returnedExp of
            Nothing -> fail "Missing returned expression"
            Just e -> return e
        let binds = st ^. lets <&> \(MkLet n expr s) -> TH.ValD ((if s then TH.BangP else id) $ TH.VarP n) (TH.NormalB expr) []
            decons = st ^. deconstructs <&> mkDeconPat
        return $ lamOrId $ TH.LetE (binds ++ decons) resExp
      where
        mkDeconPat (MkDec cName fields src count) =
            TH.ValD (TH.ConP cName [] ([0 .. count] <&> \i -> maybe TH.WildP TH.VarP (lookup i fields))) (TH.NormalB src) []
