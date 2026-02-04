{-# LANGUAGE QualifiedDo #-}

-- | In Template Haskell, a signature ('SigD') refers to a symbol's type signature with a 'Name' (e.g. _length :: [a] -> Int_). Here, by _signature_, we mean just the type signature, without a name.
module Language.Haskell.TH.Natural.Syntax.Signature (
    -- * Builder
    newSignature,
    SignatureBuilder,
    SignatureDefinition,

    -- * State
    SignatureState (..),
    tyVarBndr,
    constraints,
    params,
    result,

    -- * Functions
    addToForall,
    addConstraint,
    addParam,
    setResultType,

    -- * Re-export
    newTypeVar,
) where

import Control.Lens ((?=), (^.), (|>=))
import Control.Lens.TH
import Data.Constructor.Extract (ExtractedConstructor (fromExtractedCon))
import Language.Haskell.TH (Q, Type (AppT, ArrowT))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Internal
import qualified Language.Haskell.TH.Natural.Syntax.Internal.Builder as B
import Language.Haskell.TH.QBuilder (QBuilder, gen)
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

instance QBuilder (SignatureBuilder step Ready ()) TH.Type where
    gen = fmap fromExtractedCon . newSignature

newSignature :: SignatureBuilder step Ready () -> SignatureDefinition
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
addToForall tyVar = unsafeWithState $ tyVarBndr |>= TH.PlainTV (tyVar ^. name) TH.SpecifiedSpec

-- | Add the given type to the set of constraints
addConstraint :: (QBuilder a TH.Type) => a -> SignatureBuilder step step ()
addConstraint tyBuilder = do
    constr <- liftB $ gen tyBuilder
    unsafeWithState $
        constraints |>= constr

-- | Set the type as the nth parameter of the function's signature
--
-- (n being the number of time 'addParam' was called)
addParam :: (QBuilder a TH.Type) => a -> SignatureBuilder step step ()
addParam tyBuilder = do
    param <- liftB $ gen tyBuilder
    unsafeWithState $
        params |>= param

-- | Set the result type in the function's signature
setResultType :: (QBuilder a TH.Type) => a -> SignatureBuilder step Ready ()
setResultType tyBuilder = B.do
    resTy <- liftB $ gen tyBuilder
    unsafeWithState $ result ?= resTy
