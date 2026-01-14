module Data.Constructor.Extract.TH (extractConstructor, extractConstructorWithOptions) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Constructor.Extract.Internal
import Data.Constructor.Extract.Options
import Data.Generics
import Data.List (find)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH

extractConstructor :: Name -> DecsQ
extractConstructor name = extractConstructorWithOptions name defaultOptions

extractConstructorWithOptions :: Name -> ExtractOptions -> DecsQ
extractConstructorWithOptions name opts = do
    dataAndCon <- dataAndConFromName name
    liftIO $ print dataAndCon
    -- TODO Generate data type
    -- TODO Generate instance
    return []
