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
import Language.Haskell.TH.QBuilder (QBuilder, gen)
import Language.Haskell.TH.Syntax.ExtractedCons hiding (fName, inline)

type FuncDefinition = Q [TH.Dec]

data FuncBuilderState = MkFBS
    { _pragmas :: [TH.Pragma]
    , _dec :: FunD
    , _signature :: Maybe SigD
    }

makeLenses ''FuncBuilderState

type FuncBuilder a = ConstBuilder FuncBuilderState a

-- TODO Should not be ready if 0 clause

newFunc :: String -> FuncBuilder () -> FuncDefinition
newFunc fName builder = do
    MkFBS{..} <- runBaseBuilder builder (MkFBS [] (MkFunD (mkName fName) []) Nothing)
    return ((TH.PragmaD <$> _pragmas) ++ (fromExtractedCon <$> maybeToList _signature) ++ [fromExtractedCon _dec])

setSignature :: (QBuilder a TH.Type) => a -> FuncBuilder ()
setSignature sigBuilder = do
    sig <- liftB $ gen sigBuilder
    fName <- view (dec . name)
    signature ?= MkSigD fName sig
    return ()

-- | Add a clause to the function
addClause :: Clause -> FuncBuilder ()
addClause c = (dec . clauses) |>= c

-- | Uses an Exp as the body of a function
--
-- Warning: This operation is destructive, and replaces all previous clauses set using 'addClause'
bodyFromExp :: (QBuilder b TH.Exp) => b -> FuncBuilder ()
bodyFromExp qe = do
    e <- liftB $ gen qe
    (dec . clauses) .= [TH.Clause [] (TH.NormalB e) []]

-- | Add an inline pragma to the function
inline :: FuncBuilder ()
inline = setInline TH.Inline TH.FunLike TH.AllPhases

-- | Sets an inline pragma to the function
setInline :: TH.Inline -> TH.RuleMatch -> TH.Phases -> FuncBuilder ()
setInline i rm phs = do
    fName <- view (dec . name)
    modify $ over pragmas $ filter $ \case
        TH.InlineP{} -> False
        _ -> True
    let newInlineP = TH.InlineP fName i rm phs
    pragmas <|= newInlineP

addPragma :: TH.Pragma -> FuncBuilder ()
addPragma p = pragmas <|= p
