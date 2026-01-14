module Data.Constructor.Extract.Options (ExtractOptions (..), defaultOptions) where

import Language.Haskell.TH (Name)

data ExtractOptions = MkExtractOptions
    { newDataName :: String -> String
    -- ^ Build the name of the `data` to generate using the constructor's name given as parameter
    , newConName :: String -> String
    -- ^ Build the name of the `constructor` to generate using the original constructor's name
    , deriveClasses :: [Name]
    }

defaultOptions :: ExtractOptions
defaultOptions = MkExtractOptions{..}
  where
    newDataName = id
    newConName = ("Mk" ++)
    deriveClasses = []
