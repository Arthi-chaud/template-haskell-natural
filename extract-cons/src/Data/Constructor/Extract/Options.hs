module Data.Constructor.Extract.Options where

data ExtractOptions = MkExtractOptions
    { newDataName :: String -> String
    -- ^ Build the name of the `data` to generate using the constructor's name given as parameter
    , newConsName :: String -> String
    -- ^ Build the name of the `constructor` to generate using the original constructor's name
    }

defaultOptions :: ExtractOptions
defaultOptions = MkExtractOptions{..}
  where
    newDataName = id
    newConsName = ("Mk" ++)
