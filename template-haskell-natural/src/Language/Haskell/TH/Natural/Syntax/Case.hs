{-# LANGUAGE GADTs #-}

module Language.Haskell.TH.Natural.Syntax.Case (
    --- * Builder
    case_,
    CaseDefinition,
    CaseExprBuilder,
    --- * Functions
    matchConst,
    matchWild,
    matchList,
    matchCon,

    -- * Pattern match on constructors

    --- * Type
    ConMatchBuilder,
    ConPatternBuilder (..),
    field,
    body,
    --- * Pattern Builder
    ---- * Types
    PatternBuilder,
    Pattern,
    ---- * Functions
    var,
    constant,
    constructor,

    -- * Reexports
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Lens hiding (Empty)
import Control.Monad (replicateM)
import Data.Constructor.Extract
import Language.Haskell.TH (Exp)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Internal.Utils
import Language.Haskell.TH.Natural.Syntax.Builder
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.Syntax.ExtractedCons hiding (body)

type CaseDefinition = TH.Q CaseE

type CaseExprBuilder = ConstBuilder CaseE

case_ :: (GenExpr b) => b -> CaseExprBuilder () -> CaseDefinition
case_ q builder = do
    e <- genExpr q
    runBaseBuilder builder $ MkCaseE e []

matchConst :: (GenPat b1, GenExpr b2) => b1 -> b2 -> CaseExprBuilder ()
matchConst b1 b2 = do
    patt <- liftB $ genPat b1
    e <- liftB $ genExpr b2
    matches |>= TH.Match patt (TH.NormalB e) []

matchWild :: (GenExpr b) => b -> CaseExprBuilder ()
matchWild b = do
    e <- liftB $ genExpr b
    matches |>= TH.Match TH.WildP (TH.NormalB e) []

matchCon :: TH.Name -> ConMatchBuilder Empty Ready () -> CaseExprBuilder ()
matchCon conName cmb = do
    fCount <- liftB $ conFieldCount conName
    (MkMBS conP mexp) <- liftB $ runBaseBuilder cmb (MkMBS (MkConP conName [] (TH.WildP <$ [0 .. fCount - 1])) Nothing)
    case mexp of
        Nothing -> B.fail "Match's Expression is missing"
        Just e -> matches |>= TH.Match (fromEC conP) (TH.NormalB e) []

matchList :: (GenExpr b) => Int -> ([Exp] -> b) -> CaseExprBuilder ()
matchList listSize b = do
    fieldNames <- liftB $ replicateM listSize $ TH.newName "_i"
    let fieldExpr = TH.VarE <$> fieldNames
        fieldPats = TH.VarP <$> fieldNames
    e <- liftB $ genExpr $ b fieldExpr
    matches |>= TH.Match (TH.ListP fieldPats) (TH.NormalB e) []

--

type PatternBuilder = ConstBuilder ConP

data Pattern a where
    Var :: Pattern TH.Exp
    Constant :: (TH.Q TH.Pat) -> Pattern ()
    NestedMatch :: TH.Name -> (Int -> PatternBuilder a) -> Pattern a

var :: Pattern TH.Exp
var = Var

constructor :: TH.Name -> (Int -> PatternBuilder a) -> Pattern a
constructor = NestedMatch

constant :: (GenPat b) => b -> Pattern ()
constant = Constant . genPat

class ConPatternBuilder m where
    setFieldPattern :: Int -> TH.Pat -> m ()

field :: (ConPatternBuilder (Builder s step step)) => Int -> Pattern a -> Builder s step step a
field fidx = \case
    Var -> do
        fieldVarName <- liftB $ TH.newName "f"
        setFieldPattern fidx $ TH.VarP fieldVarName
        return $ TH.VarE fieldVarName
    Constant qpat -> liftB qpat Prelude.>>= setFieldPattern fidx
    NestedMatch conN patBuilder -> do
        fCount <- liftB $ conFieldCount conN
        (res, conP) <- liftB $ runBaseBuilder' (patBuilder fCount) (MkConP conN [] (TH.WildP <$ [0 .. fCount - 1]))
        setFieldPattern fidx $ fromEC conP
        return res

type ConMatchBuilder = Builder ConMatchBuilderState

data ConMatchBuilderState = MkMBS {_conPat :: ConP, _matchBody :: Maybe TH.Exp}

makeLenses ''ConMatchBuilderState

instance ConPatternBuilder (ConMatchBuilder step step) where
    setFieldPattern fidx patt = (conPat . pats . ix fidx) .= patt

instance ConPatternBuilder PatternBuilder where
    setFieldPattern fidx patt = (pats . ix fidx) .= patt

body :: (GenExpr b) => b -> ConMatchBuilder Empty Ready ()
body q = impure $ do
    e <- liftB $ genExpr q
    matchBody ?= e
