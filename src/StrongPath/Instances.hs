{-# OPTIONS_GHC -fno-warn-orphans #-}

module StrongPath.Instances where

import Data.Hashable
import StrongPath.FilePath
import StrongPath.Types

-- Hashable instance for Path declared here, as an orphaned instance, instead of
-- in StronPath.Internal to avoid cyclic dependency between StrongPath.FilePath
-- and StrongPath.Internal. (This cycle would arise due to the use of
-- `toFilePath` from FilePath in the instance declaration and the dependency of
-- the FilePath module on the types from the Internal module)

-- |
-- Caveat: For two relative Paths, that only differ in the Directory, that they
-- are relative to, this Hashable instance will return the same hash even though
-- they are different paths.
instance Hashable (Path s b t) where
  hashWithSalt salt = hashWithSalt salt . toFilePath