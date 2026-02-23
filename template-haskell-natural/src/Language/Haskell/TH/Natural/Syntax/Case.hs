{-# LANGUAGE GADTs #-}

-- | 'Builder' for a case expression.
-- You can find an example usage [here](https://github.com/Arthi-chaud/template-haskell-natural/tree/main/examples/packed).
module Language.Haskell.TH.Natural.Syntax.Case (
    -- * Builder
    case_,
    CaseExprBuilder,

    -- * Match
    matchConst,
    matchWild,
    matchList,
    matchCon,

    -- * Pattern match on constructors

    -- ** Type
    ConMatchBuilder,
    ConPatternBuilder (..),
    field,
    body,

    -- ** Pattern Builder

    -- *** Types
    PatternBuilder,
    Pattern,

    -- *** Functions
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

-- | A builder for the matches and branches in a case expression
type CaseExprBuilder = ConstBuilder CaseE

-- | takes an expression to pattern match on and a 'CaseExprBuilder' to produce a case expression
case_ :: (GenExpr b) => b -> CaseExprBuilder () -> TH.Q CaseE
case_ q builder = do
    e <- genExpr q
    runBaseBuilder builder $ MkCaseE e []

-- | Match on a constant expression (e.g. a literal). The second argument is the body of the match.
matchConst :: (GenPat b1, GenExpr b2) => b1 -> b2 -> CaseExprBuilder ()
matchConst b1 b2 = do
    patt <- liftB $ genPat b1
    e <- liftB $ genExpr b2
    matches |>= TH.Match patt (TH.NormalB e) []

-- | Match using a wildcard pattern. The argument is the body of the match.
matchWild :: (GenExpr b) => b -> CaseExprBuilder ()
matchWild b = do
    e <- liftB $ genExpr b
    matches |>= TH.Match TH.WildP (TH.NormalB e) []

-- | Match using a constructor. The 'ConMatchBuilder' allow deconstructing and accessing the fields of the constructor
matchCon :: TH.Name -> ConMatchBuilder Empty Ready () -> CaseExprBuilder ()
matchCon conName cmb = do
    fCount <- liftB $ conFieldCount conName
    (MkMBS conP mexp) <- liftB $ runBaseBuilder cmb (MkMBS (MkConP conName [] (TH.WildP <$ [0 .. fCount - 1])) Nothing)
    case mexp of
        Nothing -> B.fail "Match's Expression is missing"
        Just e -> matches |>= TH.Match (fromEC conP) (TH.NormalB e) []

-- | Match on a list of the given size.
-- The second argument is the body of the match, and its input is a list of 'VarE' bound to each item in the list
matchList :: (GenExpr b) => Int -> ([Exp] -> b) -> CaseExprBuilder ()
matchList listSize b = do
    fieldNames <- liftB $ replicateM listSize $ TH.newName "_i"
    let fieldExpr = TH.VarE <$> fieldNames
        fieldPats = TH.VarP <$> fieldNames
    e <- liftB $ genExpr $ b fieldExpr
    matches |>= TH.Match (TH.ListP fieldPats) (TH.NormalB e) []

type PatternBuilder = ConstBuilder ConP

data Pattern a where
    Var :: Pattern TH.Exp
    Constant :: (TH.Q TH.Pat) -> Pattern ()
    NestedMatch :: TH.Name -> (Int -> PatternBuilder a) -> Pattern a

-- | Allow binding a constructor's field to a name
var :: Pattern TH.Exp
var = Var

-- | Pattern-match a constructor's field On a nested constructor
--
-- The second argument is invoked for each field in the constructor
constructor :: TH.Name -> (Int -> PatternBuilder a) -> Pattern a
constructor = NestedMatch

-- | Pattern-match a constructor's field to a constant (e.g. a literal)
constant :: (GenPat b) => b -> Pattern ()
constant = Constant . genPat

class ConPatternBuilder m where
    setFieldPattern :: Int -> TH.Pat -> m ()

-- | In a pattern that deconstruct the value, this binds the field at the given index using the 'Pattern'
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

-- | Builds a case match for a predefined constructor (see 'matchCon')
type ConMatchBuilder = Builder ConMatchBuilderState

data ConMatchBuilderState = MkMBS {_conPat :: ConP, _matchBody :: Maybe TH.Exp}

makeLenses ''ConMatchBuilderState

instance ConPatternBuilder (ConMatchBuilder step step) where
    setFieldPattern fidx patt = (conPat . pats . ix fidx) .= patt

instance ConPatternBuilder PatternBuilder where
    setFieldPattern fidx patt = (pats . ix fidx) .= patt

-- | Sets the body of the match
body :: (GenExpr b) => b -> ConMatchBuilder Empty Ready ()
body q = impure $ do
    e <- liftB $ genExpr q
    matchBody ?= e
