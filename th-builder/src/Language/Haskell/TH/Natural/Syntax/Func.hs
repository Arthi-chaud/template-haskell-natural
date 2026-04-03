-- | 'Builder' for top-level function declaration
module Language.Haskell.TH.Natural.Syntax.Func (
    -- * Builder
    newFunc,
    FuncBuilder,

    -- * State
    FuncBuilderState (..),

    -- * Functions

    -- ** Clause
    addClause,
    bodyFromExp,

    -- ** Signature
    setSignature,

    -- ** Pragmas
    inline,
    setInline,
    addPragma,

    -- * Lenses
    pragmas,
    dec,
    signature,

    -- * Reexports
    module Language.Haskell.TH.Natural.Syntax.Builder.Monad,
) where

import Control.Lens
import Control.Monad.State
import Data.Constructor.Extract
import Data.Maybe (maybeToList)
import Language.Haskell.TH (Q, mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Builder.Monad
import Language.Haskell.TH.Syntax.ExtractedCons hiding (fName, inline)

data FuncBuilderState = MkFBS
    { _pragmas :: [TH.Pragma]
    , _dec :: FunD
    , _signature :: Maybe SigD
    }

makeLenses ''FuncBuilderState

type FuncBuilder = Builder FuncBuilderState

-- TODO Should not be ready if 0 clause

-- | Builds a function declaration. The string argument is the name of the function
newFunc :: String -> FuncBuilder step Ready () -> Q [TH.Dec]
newFunc fName builder = do
    MkFBS{..} <- runBaseBuilder builder (MkFBS [] (MkFunD (mkName fName) []) Nothing)
    return ((TH.PragmaD <$> _pragmas) ++ (fromEC <$> maybeToList _signature) ++ [fromEC _dec])

-- | Set the signature of the function
setSignature :: (GenType a) => a -> FuncBuilder step step ()
setSignature sigBuilder = do
    sig <- liftB $ genTy sigBuilder
    fName <- view (dec . name)
    signature ?= MkSigD fName sig
    return ()

-- | Add a clause to the function
addClause :: Clause -> FuncBuilder step Ready ()
addClause c = impure $ (dec . clauses) |>= c

-- | Uses an Exp as the body of a function
--
-- Warning: This operation is destructive, and replaces all previous clauses set using 'addClause'
bodyFromExp :: (GenExpr b) => b -> FuncBuilder step Ready ()
bodyFromExp qe = impure $ do
    e <- liftB $ genExpr qe
    (dec . clauses) .= [TH.Clause [] (TH.NormalB e) []]

-- | Add an inline pragma to the function
inline :: FuncBuilder step step ()
inline = setInline TH.Inline TH.FunLike TH.AllPhases

-- | Sets an inline pragma to the function
setInline :: TH.Inline -> TH.RuleMatch -> TH.Phases -> FuncBuilder step step ()
setInline i rm phs = do
    fName <- view (dec . name)
    modify $ over pragmas $ filter $ \case
        TH.InlineP{} -> False
        _ -> True
    let newInlineP = TH.InlineP fName i rm phs
    pragmas <|= newInlineP

-- | Add a 'Pragma' alongside the function declaration
addPragma :: TH.Pragma -> FuncBuilder step step ()
addPragma p = pragmas <|= p
