module Language.Haskell.TH.Natural.Syntax.Expr.Do where

import Language.Haskell.TH.Natural.Syntax.Expr.Simple

import Control.Lens
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Class
import Language.Haskell.TH.Natural.Syntax.Expr.Class
import Language.Haskell.TH.Natural.Syntax.Internal.Builder
import Language.Haskell.TH.Syntax (ModName (..), nameBase)
import Language.Haskell.TH.Syntax.ExtractedCons

type DoExprDefinition = TH.Q DoE

newtype DoBinding = MkDoBind LetBinding deriving (Eq, Show)

type DoStmt = TH.Exp

data DoExprStep = Bind DoBinding | Stmt DoStmt | Let LetBinding | Decons Deconstruct

newtype DoExprBuilderState
    = -- | Works as a stack: the last step is the first in the list
      MkDoEBS {_steps :: [DoExprStep]}

makeLenses ''DoExprBuilderState

type DoExprBuilder = Builder DoExprBuilderState

newQualifiedDo :: TH.Name -> DoExprBuilder step Ready () -> DoExprDefinition
newQualifiedDo modN builder = do
    doE <- runExprBuilder builder
    return $ over modName (const $ Just $ ModName $ nameBase modN) doE

newDo :: DoExprBuilder step Ready () -> DoExprDefinition
newDo = runExprBuilder

instance ExprBuilder DoExprBuilder where
    type Definition DoExprBuilder = DoExprDefinition
    returns q = unsafeCastStep $ do
        exp <- liftB $ gen q
        steps <|= Stmt exp

    runExprBuilder b = do
        st <- runBaseBuilder b (MkDoEBS [])
        undefined -- TODO
