{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr (
    -- * Types
    ExprDefinition,

    -- * Builder
    ExprBuilder,
    runExprBuilder,

    -- * Operations
    arg,
    returns,
    let_,
) where

import Control.Lens (makeLenses, views, (?=), (^.), (|>=))
import Control.Monad.Reader
import Data.Functor ((<&>))
import Language.Haskell.TH (Q, TExp, mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder, gen)
import Language.Haskell.TH.Natural.Syntax.Internal hiding (liftQ, (>>=))
import Language.Haskell.TH.Natural.Syntax.Internal.Builder (liftQ)
import Language.Haskell.TH.Syntax (TExp (TExp))
import Prelude hiding ((>>=))

type ExprDefinition a = Q (TExp a)

data ExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _lets :: [(TH.Name, TH.Exp)]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''ExprBuilderState

type ExprBuilder prev next a = Builder ExprBuilderState prev next a

runExprBuilder :: ExprBuilder prev Ready () -> ExprDefinition t
runExprBuilder b = do
    st <- runBaseBuilder b (MkEBS [] [] Nothing)
    let lamOrId = case st ^. argNames of
            [] -> id
            names -> TH.LamE (TH.VarP <$> names)
    resExp <- case st ^. returnedExp of
        Nothing -> fail "Missing returned expression"
        Just e -> return e
    let letDecs = st ^. lets <&> \(n, expr) -> TH.ValD (TH.VarP n) (TH.NormalB expr) []
    return $ TExp $ lamOrId $ TH.LetE letDecs resExp

arg :: ExprBuilder curr curr TH.Exp
arg = do
    prevArgCount <- views argNames length
    let nextArgName = mkName $ 'a' : show (prevArgCount + 1)
    argNames |>= nextArgName
    return $ TH.VarE nextArgName

returns :: (THBuilder b TH.Exp) => b -> ExprBuilder curr Ready ()
returns q = unsafeCastStep $ do
    expr <- liftQ $ gen q
    returnedExp ?= expr

let_ :: forall b t' step. (THBuilder b (TExp t')) => b -> ExprBuilder step step (TExp t')
let_ b = do
    prevLetCount <- views lets length
    varName <- lift $ TH.newName ("var" ++ show prevLetCount)
    TExp expr <- lift $ gen @b @(TExp t') b
    lets |>= (varName, expr)
    return $ TExp $ TH.VarE varName

-- TODO do notation
-- TODO Avoid having to do type application on gen
