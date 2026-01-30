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
    liftQ,
    unsafeWithBuilder,

    -- * Operations
    arg,
    returns,
    let_,
) where

import Control.Lens (makeLenses, views, (?=), (^.), (|>=))
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor ((<&>))
import Language.Haskell.TH (Q, TExp, mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder, gen)
import Language.Haskell.TH.Natural.Syntax.Internal hiding (liftQ, (>>=))
import Language.Haskell.TH.Syntax (TExp (TExp))
import Prelude hiding ((>>=))
import qualified Prelude

type ExprDefinition a = Q (TExp a)

data ExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _lets :: [(TH.Name, TH.Exp)]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''ExprBuilderState

newtype ExprBuilder t prev next a = MkEB {unEB :: ConstBuilder ExprBuilderState a} deriving (Functor)

runExprBuilder :: ExprBuilder t prev Ready () -> ExprDefinition t
runExprBuilder (MkEB b) = do
    st <- runBaseBuilder b (MkEBS [] [] Nothing)
    let lamOrId = case st ^. argNames of
            [] -> id
            names -> TH.LamE (TH.VarP <$> names)
    resExp <- case st ^. returnedExp of
        Nothing -> fail "Missing returned expression"
        Just e -> return e
    let letDecs = st ^. lets <&> \(n, expr) -> TH.ValD (TH.VarP n) (TH.NormalB expr) []
    return $ TExp $ lamOrId $ TH.LetE letDecs resExp

unsafeWithBuilder :: ConstBuilder ExprBuilderState a -> ExprBuilder t prev next a
unsafeWithBuilder = MkEB

instance Applicative (ExprBuilder t step step) where
    pure a = MkEB $ pure a
    liftA2 pair (MkEB f1) (MkEB f2) = MkEB (liftA2 pair f1 f2)

instance Monad (ExprBuilder t step step) where
    (>>=) (MkEB f1) f2 = MkEB $ f1 Prelude.>>= unEB . f2

(>>=) :: ExprBuilder t prev curr a -> (a -> ExprBuilder t curr next b) -> ExprBuilder t prev next b
(>>=) (MkEB (MkB f1)) f2 = MkEB $ MkB $ f1 Prelude.>>= \a -> unB (unEB $ f2 a)

instance MonadReader ExprBuilderState (ExprBuilder t step step) where
    ask = MkEB $ MkB get
    local f m = MkEB (MkB (modify f)) >>= const m

liftQ :: Q a -> ExprBuilder t step step a
liftQ q = MkEB $ lift q

arg :: forall curr t a. ExprBuilder t curr curr (TExp a)
arg = unsafeWithBuilder $ do
    prevArgCount <- views argNames length
    let nextArgName = mkName $ 'a' : show (prevArgCount + 1)
    argNames |>= nextArgName
    return $ TExp $ TH.VarE nextArgName

returns :: forall curr t t' b. (THBuilder b (TExp t')) => b -> ExprBuilder t curr Ready ()
returns q = unsafeWithBuilder $ do
    TExp expr <- lift $ gen @b @(TExp t') q
    returnedExp ?= expr

let_ :: forall b t' t step. (THBuilder b (TExp t')) => b -> ExprBuilder t step step (TExp t')
let_ b = unsafeWithBuilder $ do
    prevLetCount <- views lets length
    varName <- lift $ TH.newName ("var" ++ show prevLetCount)
    TExp expr <- lift $ gen @b @(TExp t') b
    lets |>= (varName, expr)
    return $ TExp $ TH.VarE varName

-- TODO do notation
-- TODO Avoid having to do type application on gen
--
--
-- type family ConsumableArg a where
--     ConsumableArg (a ': b) = a
--
-- type family ConsumedArg a where
--     ConsumedArg (_ ': b) = b
