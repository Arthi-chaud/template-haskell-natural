{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.Haskell.TH.Natural.Syntax.Expr (
    -- * Types
    ExprDefinition,
    -- ExprBuilder,
) where

import Control.Lens (makeLenses, use, uses, views, (%~), (.=), (?=), (^.), (|>), (|>=))
import Control.Monad.Reader
import Control.Monad.State
import Data.Coerce
import Language.Haskell.TH (Q, TExp, mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder, gen)
import Language.Haskell.TH.Natural.Syntax.Internal hiding ((>>=))
import qualified Language.Haskell.TH.Natural.Syntax.Internal.Builder as B
import Language.Haskell.TH.Syntax (TExp (TExp))
import Prelude hiding ((>>=))
import qualified Prelude

type ExprDefinition a = Q (TExp a)

data ExprBuilderState = MkEBS
    { _argNames :: [TH.Name]
    , _binds :: [(Name, TH.Exp)]
    , _returnedExp :: Maybe TH.Exp
    }

makeLenses ''ExprBuilderState

newtype ExprBuilder t prev next a = MkEB {unEB :: Builder ExprBuilderState prev next a} deriving (Functor)

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

type family ConsumableArg a where
    ConsumableArg (a ': b) = a

type family ConsumedArg a where
    ConsumedArg (_ ': b) = b

arg :: forall curr rest a b t. (ConsumableArg curr ~ a, ConsumedArg curr ~ b) => ExprBuilder t curr rest (TExp a)
arg =
    MkEB $
        unsafeCastStep @curr @rest @curr @curr
            ( Prelude.do
                prevArgCount <- views argNames length
                let nextArgName = mkName $ 'a' : show (prevArgCount + 1)
                unsafeWithState $
                    argNames |>= nextArgName
                return $ TExp $ TH.VarE nextArgName
            )

returns :: forall curr t t' b. (THBuilder b (TExp t'), curr ~ '[t']) => b -> ExprBuilder t curr '[] ()
returns q =
    MkEB $
        unsafeCastStep @curr @'[] @curr @curr
            ( Prelude.do
                TExp expr <- lift $ gen @b @(TExp t') q
                unsafeWithState $
                    returnedExp ?= expr
            )

-- TODO runExpBuilder, let_, do notation
