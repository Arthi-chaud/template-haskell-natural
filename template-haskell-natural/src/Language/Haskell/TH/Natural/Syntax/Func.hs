module Language.Haskell.TH.Natural.Syntax.Func (
    -- * Builder
    FuncDefinition,
    FuncBuilder,

    -- * State
    FuncBuilderState (..),

    -- * Functions
    newFunc,
    --- * Clause
    addClause,
    bodyFromExp,
    --- * Signature
    setSignature,
    --- * Pragmas
    inline,
    setInline,
    addPragma,
) where

import Control.Lens
import Control.Monad.State
import Data.Constructor.Extract
import Data.Maybe (maybeToList)
import Language.Haskell.TH (Q, mkName)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons hiding (inline)

type FuncDefinition = Q [TH.Dec]

data FuncBuilderState = MkFBS
    { _pragmas :: [TH.Pragma]
    , _dec :: FunD
    , _signature :: Maybe SigD
    }

makeLenses ''FuncBuilderState

type FuncBuilder a = ConstBuilder FuncBuilderState a

-- TODO Should not be ready if 0 clause

setSignature :: SigD -> FuncBuilder ()
setSignature s = signature .= Just s

newFunc :: String -> FuncBuilder () -> FuncDefinition
newFunc fName builder = do
    MkFBS{..} <- runBaseConstBuilder builder (MkFBS [] (MkFunD (mkName fName) []) Nothing)
    return (fromExtractedCon _dec : (fromExtractedCon <$> maybeToList _signature) ++ (TH.PragmaD <$> _pragmas))

-- | Add a clause to the function
addClause :: Clause -> FuncBuilder ()
addClause c = (dec . clauses) %= (++ [c])

-- | Uses an Exp as the body of a function
--
-- Warning: This operation is destructive, and replaces all previous clauses set using 'addClause'
bodyFromExp :: Q TH.Exp -> FuncBuilder ()
bodyFromExp qe = do
    e <- lift qe
    (dec . clauses) .= [TH.Clause [] (TH.NormalB e) []]

-- | Add an inline pragma to the function
inline :: FuncBuilder ()
inline = setInline TH.Inline TH.FunLike TH.AllPhases

-- | Sets an inline pragma to the function
setInline :: TH.Inline -> TH.RuleMatch -> TH.Phases -> FuncBuilder ()
setInline i rm phs = do
    fName <- gets (^. (dec . name))
    modify $ over pragmas $ filter $ \case
        TH.InlineP{} -> False
        _ -> True
    let newInlineP = TH.InlineP fName i rm phs
    modify $ over pragmas (newInlineP :)

addPragma :: TH.Pragma -> FuncBuilder ()
addPragma p = modify $ over pragmas (p :)
