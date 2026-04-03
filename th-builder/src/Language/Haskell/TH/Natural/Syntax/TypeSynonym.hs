{-# LANGUAGE QualifiedDo #-}

-- | Builder for type synonyms
module Language.Haskell.TH.Natural.Syntax.TypeSynonym (
    -- * Type
    newTypeSynonym,
    TypeSynonymBuilder,
    TypeSynonymBuilderState (..),

    -- * Functions
    addTypeVar,
    addTypeVar',
    returns,

    -- * Lenses
    resType,
    tyVars,
) where

import Control.Lens
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Syntax.ExtractedCons (HasTyVarBndr (..), TySynD (MkTySynD))

type TypeSynonymBuilder prev next a = Builder TypeSynonymBuilderState prev next a

data TypeSynonymBuilderState = MkTSBS
    { _resType :: Maybe TH.Type
    , _tyVars :: [TyVarBndr BndrVis]
    }

makeLenses ''TypeSynonymBuilderState

instance HasTyVarBndr TypeSynonymBuilderState [TyVarBndr BndrVis] where
    tyVarBndr = tyVars

-- | Builds a type synonym. The first argument is the name of the type synonym
newTypeSynonym :: String -> TypeSynonymBuilder step Ready () -> Q TySynD
newTypeSynonym synName builder = do
    MkTSBS{..} <- runBaseBuilder builder (MkTSBS Nothing [])
    resTy <- case _resType of
        Nothing -> Prelude.fail "The type synonym does not have a type."
        Just r -> return r
    return $ MkTySynD (TH.mkName synName) _tyVars resTy

-- | Sets the RHS of the type synonym definition
returns :: (GenType b) => b -> TypeSynonymBuilder step Ready ()
returns b = impure $ B.do
    ty <- liftB $ genTy b
    resType ?= ty
