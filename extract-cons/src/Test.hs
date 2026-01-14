{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}

module Test where

import Data.Coerce (Coercible, coerce)
import Data.Constructor.Extract.TH
import Data.Type.Coercion (coerceWith)

extractConstructor 'Nothing
extractConstructor 'Just

-- data D = D
-- extractConstructor 'D
data Nothing = MkNothing

-- instance Coercible Nothing (Maybe a)

-- x :: Maybe a
-- x = coerceWith MkNothing
