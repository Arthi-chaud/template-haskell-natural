{-# LANGUAGE QualifiedDo #-}

module Data.Packed.NaturalTH.Packable (genPackable) where

import Control.Monad
import qualified Data.Packed as R
import Data.Packed.TH.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Natural.Syntax.Builder
import qualified Language.Haskell.TH.Natural.Syntax.Builder as B
import Language.Haskell.TH.Natural.Syntax.Common
import Language.Haskell.TH.Natural.Syntax.Func
import Language.Haskell.TH.Natural.Syntax.Instance
import Language.Haskell.TH.QBuilder

genPackable :: Name -> Q Dec
genPackable tyName = gen $ newInstance ''R.Packable $ B.do
    (resolvedType, typeVariables) <- liftB $ resolveAppliedType tyName
    forM_ typeVariables $ \tyVar ->
        addContext [t|R.Packable $(varT tyVar)|]
    addInstanceArg $ pure resolvedType
    addBody' $ newFunc "write" $ bodyFromExp $ varE (mkName $ "write" ++ nameBase tyName)
