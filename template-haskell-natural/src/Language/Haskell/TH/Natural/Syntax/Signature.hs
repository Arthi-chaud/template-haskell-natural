{-# LANGUAGE QualifiedDo #-}

-- | In Template Haskell, a signature ('SigD') refers to a symbol's type signature with a 'Name' (e.g. _length :: [a] -> Int_). Here, by _signature_, we mean just the type signature, without a name.
module Language.Haskell.TH.Natural.Syntax.Signature (
    -- * Types
    SignatureDefinition,
    SignatureBuilder,

    -- * State
    SignatureState (..),
    tyVarBndr,
    constraints,
    params,
    result,

    -- * Functions
    newSignature,
    newTypeVar,
    addToForall,
    addConstraint,
    addParam,
    setResultType,
) where

import Control.Lens ((?=), (|>=))
import Control.Lens.TH
import Data.Coerce
import Language.Haskell.TH (Q, Type (AppT, ArrowT))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class (THBuilder, gen)
import Language.Haskell.TH.Natural.Syntax.Internal
import qualified Language.Haskell.TH.Natural.Syntax.Internal.Builder as B
import Language.Haskell.TH.Syntax.ExtractedCons hiding (inline, tyVarBndr)

type SignatureDefinition = Q ForallT

type SignatureBuilder prev next a = Builder SignatureState prev next a

data SignatureState = MkSBS
    { _tyVarBndr :: [TH.TyVarBndr TH.Specificity]
    , _constraints :: [TH.Type]
    , _params :: [TH.Type]
    , _result :: Maybe TH.Type
    }

makeLenses ''SignatureState

newSignature :: SignatureBuilder Empty Ready () -> SignatureDefinition
newSignature builder = do
    MkSBS{..} <- runBaseBuilder builder (MkSBS [] [] [] Nothing)
    resTy <- case _result of
        Nothing -> fail "The signature does not contain a return type."
        Just r -> return r
    let funcType = foldr (\param -> ((ArrowT `AppT` param) `AppT`)) resTy _params
    return $ MkForallT _tyVarBndr _constraints funcType

-- | Adds the given type variable to the _forall_ list.
--
-- Using this function should comply with the 'forall-or-nothing' rule (https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html#the-forall-or-nothing-rule)
addToForall :: TypeVarName -> SignatureBuilder step step ()
addToForall tyVar = unsafeWithState $ tyVarBndr |>= TH.PlainTV (coerce tyVar) TH.SpecifiedSpec

-- | Add the given type to the set of constraints
addConstraint :: (THBuilder a TH.Type) => a -> SignatureBuilder step step ()
addConstraint tyBuilder = do
    constr <- liftB $ gen tyBuilder
    unsafeWithState $
        constraints |>= constr

-- | Set the type as the nth parameter of the function's signature
--
-- (n being the number of time 'addParam' was called)
addParam :: (THBuilder a TH.Type) => a -> SignatureBuilder step step ()
addParam tyBuilder = do
    param <- liftB $ gen tyBuilder
    unsafeWithState $
        params |>= param

-- | Set the result type in the function's signature
setResultType :: (THBuilder a TH.Type) => a -> SignatureBuilder step Ready ()
setResultType tyBuilder = B.do
    resTy <- liftB $ gen tyBuilder
    unsafeWithState $ result ?= resTy
