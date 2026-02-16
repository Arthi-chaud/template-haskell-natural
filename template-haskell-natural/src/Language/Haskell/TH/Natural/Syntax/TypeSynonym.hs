{-# LANGUAGE QualifiedDo #-}

module Language.Haskell.TH.Natural.Syntax.TypeSynonym (
    -- * Type
    newTypeSynonym,
    TypeSynonymBuilder,
    TypeSynonymBuilderState (..),
    TypeSynonymDefinition,

    -- * Functions
    addTypeVar,
    addTypeVar',
    returns,
) where

import Control.Lens
import Language.Haskell.TH
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Name
import Language.Haskell.TH.Syntax.ExtractedCons (HasName (..), TySynD (MkTySynD))

type TypeSynonymDefinition = Q TySynD

type TypeSynonymBuilder prev next a = Builder TypeSynonymBuilderState prev next a

data TypeSynonymBuilderState = MkTSBS
    { _resType :: Maybe TH.Type
    , _tyVars :: [TyVarBndr BndrVis]
    }

makeLenses ''TypeSynonymBuilderState

newTypeSynonym :: String -> TypeSynonymBuilder step Ready () -> TypeSynonymDefinition
newTypeSynonym synName builder = do
    MkTSBS{..} <- runBaseBuilder builder (MkTSBS Nothing [])
    resTy <- case _resType of
        Nothing -> Prelude.fail "The type synonym does not have a type."
        Just r -> return r
    return $ MkTySynD (TH.mkName synName) _tyVars resTy

addTypeVar :: TypeVarName -> TypeSynonymBuilder step step ()
addTypeVar tyN = addTypeVar' tyN BndrReq Nothing

addTypeVar' :: TypeVarName -> BndrVis -> Maybe TH.Kind -> TypeSynonymBuilder step step ()
addTypeVar' tyN vis mkind =
    tyVars |>= maybe (PlainTV n vis) (KindedTV n vis) mkind
  where
    n = tyN ^. name

returns :: (GenType b) => b -> TypeSynonymBuilder step Ready ()
returns b = impure $ B.do
    ty <- liftB $ genTy b
    resType ?= ty
