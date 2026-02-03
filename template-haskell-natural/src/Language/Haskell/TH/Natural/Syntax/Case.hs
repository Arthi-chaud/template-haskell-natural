{-# LANGUAGE GADTs #-}

module Language.Haskell.TH.Natural.Syntax.Case (
    -- * Case Expression

    --- * Types
    CaseDefinition,
    CaseExprBuilder,
    --- * Functions
    case_,
    matchConst,
    matchWild,
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
) where

import Control.Lens hiding (Empty)
import Data.Constructor.Extract (ExtractedConstructor (fromExtractedCon))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class
import Language.Haskell.TH.Natural.Syntax.Internal.Builder
import Language.Haskell.TH.Natural.Syntax.Internal.Utils (conFieldCount)
import Language.Haskell.TH.Syntax.ExtractedCons hiding (body)

type CaseDefinition = TH.Q CaseE

type CaseExprBuilder = ConstBuilder CaseE

case_ :: (THBuilder b TH.Exp) => b -> CaseExprBuilder () -> CaseDefinition
case_ q builder = do
    e <- gen q
    runBaseBuilder builder $ MkCaseE e []

matchConst :: ((THBuilder b1 TH.Pat), THBuilder b2 TH.Exp) => b1 -> b2 -> CaseExprBuilder ()
matchConst b1 b2 = do
    patt <- liftB $ gen b1
    e <- liftB $ gen b2
    matches |>= TH.Match patt (TH.NormalB e) []

matchWild :: (THBuilder b TH.Exp) => b -> CaseExprBuilder ()
matchWild b = do
    e <- liftB $ gen b
    matches |>= TH.Match TH.WildP (TH.NormalB e) []

matchCon :: TH.Name -> ConMatchBuilder Empty Ready () -> CaseExprBuilder ()
matchCon conName cmb = do
    fCount <- liftB $ conFieldCount conName
    (MkMBS conP mexp) <- liftB $ runBaseBuilder cmb (MkMBS (MkConP conName [] (TH.WildP <$ [0 .. fCount])) Nothing)
    case mexp of
        Nothing -> fail "Match's Expression is missing"
        Just e -> matches |>= TH.Match (fromExtractedCon conP) (TH.NormalB e) []

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

constant :: (THBuilder b TH.Pat) => b -> Pattern ()
constant = Constant . gen

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
        (res, conP) <- liftB $ runBaseBuilder' (patBuilder fCount) (MkConP conN [] (TH.WildP <$ [0 .. fCount]))
        setFieldPattern fidx $ fromExtractedCon conP
        return res

type ConMatchBuilder = Builder ConMatchBuilderState

data ConMatchBuilderState = MkMBS {_conPat :: ConP, _matchBody :: Maybe TH.Exp}

makeLenses ''ConMatchBuilderState

instance ConPatternBuilder (ConMatchBuilder step step) where
    setFieldPattern fidx patt = (conPat . pats . ix fidx) .= patt

instance ConPatternBuilder PatternBuilder where
    setFieldPattern fidx patt = (pats . ix fidx) .= patt

body :: (THBuilder b TH.Exp) => b -> ConMatchBuilder Empty Ready ()
body q = impure $ do
    e <- liftB (gen q)
    matchBody ?= e
