module Data.Constructor.Extract.Class (ExtractedConstructor (..)) where

-- | Type classes (whose instances will be derived using TH) that allows going back and forth between the extracted constructor type (con) and the source data type (ty)
class ExtractedConstructor con ty where
    fromExtractedCon :: con -> ty
    toExtractedCon :: ty -> Maybe con
