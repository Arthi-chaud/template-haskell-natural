{-# LANGUAGE QualifiedDo #-}

module Data.Packed.NaturalTH.Unpackable (genUnpackable) where

import Control.Monad
import qualified Data.Packed as R
import Data.Packed.TH.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Gen
import Language.Haskell.TH.Natural.Syntax.Builder
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Instance

genUnpackable :: Name -> Q Dec
genUnpackable tyName = genDec $ newInstance ''R.Unpackable $ B.do
    (resolvedType, typeVariables) <- liftB $ resolveAppliedType tyName
    forM_ typeVariables $ \tyVar ->
        addContext [t|R.Unpackable $(varT tyVar)|]
    addInstanceArg resolvedType
    addBody' $ newFunc "reader" $ bodyFromExp $ VarE (mkName $ "read" ++ nameBase tyName)
