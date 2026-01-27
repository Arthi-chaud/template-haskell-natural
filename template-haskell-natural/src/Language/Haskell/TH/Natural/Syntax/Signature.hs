module Language.Haskell.TH.Natural.Syntax.Signature (
    -- * Types
    SignatureDefinition,
    SignatureBuilder,

    -- * State
    SignatureState (..),

    -- * Functions
    signature,
) where

import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons hiding (inline)

type SignatureDefinition = Q SigD

type SignatureBuilder a = Builder SignatureState Ready Ready a

data SignatureState st = MkSBS {params :: [TH.Type], constraints :: [TH.Type], result :: StepType st TH.Type}

signature :: SignatureBuilder () -> SignatureDefinition
signature = undefined
