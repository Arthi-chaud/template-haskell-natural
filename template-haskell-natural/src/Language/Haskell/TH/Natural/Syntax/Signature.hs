module Language.Haskell.TH.Natural.Syntax.Signature where

import Language.Haskell.TH (Q)
import Language.Haskell.TH.Natural.Syntax.Internal
import Language.Haskell.TH.Syntax.ExtractedCons hiding (inline)

type SignatureDefinition = Q SigD

type SignatureBuilder a = Builder SigD a

-- TODO
