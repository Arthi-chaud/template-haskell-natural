module Language.Haskell.TH.Natural.Syntax.Expr.Do (
    -- * Types
    newDo,
    newQualifiedDo,
    DoExprDefinition,
    DoExprBuilder,

    -- * State
    DoExprBuilderState (..),
    DoExprStep (..),

    -- * Functions
    stmt,
    bind,
    strictBind,
    bind_,

    -- * Reexport
    module Language.Haskell.TH.Natural.Syntax.Expr.Class,
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Applicative
import Control.Arrow (second)
import Control.Lens hiding (Empty)
import Control.Monad (forM)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.Natural.Syntax.Expr.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Internal
import Language.Haskell.TH.QBuilder
import Language.Haskell.TH.Syntax (ModName (..), nameBase)
import Language.Haskell.TH.Syntax.ExtractedCons hiding (expr)

type DoExprDefinition = TH.Q DoE

data DoExprStep = Bind Binding | Stmt TH.Exp | Let Binding | Decons Deconstruct

newtype DoExprBuilderState
    = -- | Works as a stack: the last step is the first in the list
      MkDoEBS {_steps :: [DoExprStep]}

makeLenses ''DoExprBuilderState

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

instance ExprBuilder DoExprBuilder where
    type Definition DoExprBuilder = DoExprDefinition
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
    withDeconstruct expr f = impure $ do
        prevSteps <- view steps
        (newSteps, mres) <-
            second asum . unzip
                <$> forM
                    prevSteps
                    ( \case
                        Decons d
                            | _src d == expr -> do
                                (res, d') <- f $ Just d
                                pure (Decons d', Just res)
                        s -> pure (s, Nothing)
                    )
        case mres of
            Nothing -> do
                (res, newDecons) <- f Nothing
                steps .= (Decons newDecons : newSteps)
                return res
            Just res -> do
                steps .= newSteps
                return res

    addLet b = impure $ steps |>= Let b

    runExprBuilder b = do
        MkDoEBS doSteps <- runBaseBuilder b (MkDoEBS [])
        return $ MkDoE Nothing $ reverse $ fmap stepToStmt doSteps
      where
        stepToStmt = \case
            Bind (MkBind n e s) -> TH.BindS ((if s then TH.BangP else id) $ TH.VarP n) e
            Stmt e -> TH.NoBindS e
            Let let_ -> TH.LetS [bindingToDec let_]
            Decons decons -> TH.LetS [deconstructToDec decons]
