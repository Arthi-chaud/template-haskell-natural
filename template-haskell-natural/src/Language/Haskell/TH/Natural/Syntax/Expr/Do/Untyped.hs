{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Haskell.TH.Natural.Syntax.Expr.Do.Untyped (
    -- * Types
    newDo,
    newQualifiedDo,
    DoExprDefinition,
    DoExprBuilder,

    -- * State
    module Language.Haskell.TH.Natural.Syntax.Expr.Do.State,

    -- * Functions
    stmt,
    bind,
    strictBind,
    bind_,

    -- * Reexport
    module Language.Haskell.TH.Natural.Syntax.Expr.Class,
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Lens hiding (Empty)
import Control.Monad (foldM)
import Data.Bifunctor
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.Natural.Syntax.Expr.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Do.State
import Language.Haskell.TH.Natural.Syntax.Expr.Internal
import Language.Haskell.TH.QBuilder
import Language.Haskell.TH.Syntax (ModName (..), nameBase)
import Language.Haskell.TH.Syntax.ExtractedCons hiding (expr)

type DoExprDefinition = TH.Q DoE
type DoExprBuilder = Builder DoExprBuilderState

newQualifiedDo :: TH.Name -> DoExprBuilder step Ready () -> DoExprDefinition
newQualifiedDo modN builder = do
    doE <- runExprBuilder builder
    return $ over modName (const $ Just $ ModName $ nameBase modN) doE

newDo :: DoExprBuilder step Ready () -> DoExprDefinition
newDo = runExprBuilder

stmt :: (QBuilder b TH.Exp) => b -> DoExprBuilder step Ready ()
stmt = returns

strictBind :: (QBuilder b TH.Exp) => b -> DoExprBuilder step Empty TH.Exp
strictBind = bind_ True

bind :: (QBuilder b TH.Exp) => b -> DoExprBuilder step Empty TH.Exp
bind = bind_ False

bind_ :: (QBuilder b TH.Exp) => Bool -> b -> DoExprBuilder step Empty TH.Exp
bind_ s q = impure $ do
    stepCount <-
        views steps $
            length
                . filter
                    ( \case
                        Bind _ -> True
                        _ -> False
                    )
    varName <- liftB $ TH.newName ("var" ++ show stepCount)
    e <- liftB $ gen q
    steps <|= Bind (MkBind varName e s)
    return $ TH.VarE varName

instance IsExprBuilder DoExprBuilderState where
    type Definition DoExprBuilderState = DoExprDefinition
    returns q = unsafeCastStep $ do
        e <- liftB $ gen q
        steps <|= Stmt e

    letCount =
        views steps $
            length
                . filter
                    ( \case
                        Let _ -> True
                        _ -> False
                    )
    addDeconstruct d = impure $ steps <|= Decons d

    addLet b = impure $ steps <|= Let b

    runExprBuilder b = do
        MkDoEBS doSteps <- runBaseBuilder b (MkDoEBS [])
        mergedSteps <- _compileDoExpr $ reverse doSteps
        return $ MkDoE Nothing $ fmap stepToStmt mergedSteps
      where
        stepToStmt = \case
            Bind (MkBind n e s) -> TH.BindS ((if s then TH.BangP else id) $ TH.VarP n) e
            Stmt e -> TH.NoBindS e
            Let let_ -> TH.LetS [bindingToDec let_]
            Decons decons -> TH.LetS [deconstructToDec decons]

-- | Merges deconstructions of common expressions together
_compileDoExpr :: (MonadFail m) => [DoExprStep] -> m [DoExprStep]
_compileDoExpr = mergeDeconsSteps
  where
    -- TODO: Merge Bind and decons, but 'Binding' uses a name for the pattern
    mergeDeconsSteps [] = pure []
    mergeDeconsSteps (Decons d : rest) = case partitionMatchingDecons (_src d) rest of
        ([], _) -> (Decons d :) <$> mergeDeconsSteps rest
        (ds, rest') -> do
            d' <- Decons <$> foldM mergeDeconstruct d ds
            (d' :) <$> mergeDeconsSteps rest'
    mergeDeconsSteps (s : rest) = (s :) <$> mergeDeconsSteps rest
    partitionMatchingDecons :: TH.Exp -> [DoExprStep] -> ([Deconstruct], [DoExprStep])
    partitionMatchingDecons _ [] = ([], [])
    partitionMatchingDecons e (Decons d : r) =
        if _src d == e
            then first (d :) $ partitionMatchingDecons e r
            else second (Decons d :) $ partitionMatchingDecons e r
    partitionMatchingDecons e (s : r) = second (s :) $ partitionMatchingDecons e r
