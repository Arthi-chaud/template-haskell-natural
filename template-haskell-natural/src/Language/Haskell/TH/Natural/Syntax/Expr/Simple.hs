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
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Lens (makeLenses, views, (?=), (^.), (|>=))
import Control.Monad
import Data.Bifunctor
import Data.Functor ((<&>))
import Data.List (partition)
import Data.Maybe
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder hiding (fail, (>>=))
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
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

    addDeconstruct d = impure $ deconstructs |>= d

    addLet l = impure $ lets |>= l

    letCount = views lets length

    runExprBuilder b = do
        st <- runBaseBuilder b (MkEBS [] [] [] Nothing)
        (argsPat, decs) <- _compileSimpleExpr st
        let lamOrId = case argsPat of
                [] -> id
                _ -> TH.LamE argsPat
        let letOrId = case decs of
                [] -> id
                _ -> TH.LetE decs
        resExp <- case st ^. returnedExp of
            Nothing -> Prelude.fail "Missing returned expression"
            Just e -> return e
        return $ lamOrId $ letOrId resExp

-- | Merges deconstructions of common expressions together, and returns a pair where:
--
-- - The first element is the patterns for the args (if any), deconstructed or not according to the decons
-- - The list of declarations (i.e. lets and deconstructs) to pass to 'LetE'
_compileSimpleExpr :: (MonadFail m) => SimpleExprBuilderState -> m ([TH.Pat], [TH.Dec])
_compileSimpleExpr st = do
    mergedDecons <- mergeDeconstructs $ st ^. deconstructs
    let (argPats, decons') =
            foldr
                (\argName (argsPats, decons) -> first (: argsPats) $ argToPat argName decons)
                ([], mergedDecons)
                (st ^. argNames)
        bindDecs = st ^. lets <&> bindingToDec
        deconDecs = decons' <&> deconstructToDec
    return (argPats, bindDecs ++ deconDecs)
  where
    argToPat :: TH.Name -> [Deconstruct] -> (TH.Pat, [Deconstruct])
    argToPat n decons = case partition (\d -> _src d == nExp) decons of
        ([], _) -> (TH.VarP n, decons)
        -- Only considering one matching decons, this function should be called using merged decons.
        (match : decons', decons'') -> (deconstructToPat match, decons' ++ decons'')
      where
        nExp = TH.VarE n
