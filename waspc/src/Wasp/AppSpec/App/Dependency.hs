{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Dependency
  ( Dependency (..),
    fromList,
    make,
  )
where

import Data.Aeson
import Data.Data (Data)
import GHC.Generics

data Dependency = Dependency
  { name :: String,
    -- | NOTE: By npm docs, this can be semver version range,
    -- but it can also be a URL (tarball, git or Github), or a local file path.
    version :: String
  }
  deriving (Show, Eq, Data, Generic)

instance ToJSON Dependency where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Dependency

instance Ord Dependency where
  a <= b = name a <= name b

fromList :: [(String, String)] -> [Dependency]
fromList = map make

make :: (String, String) -> Dependency
make (n, v) = Dependency {name = n, version = v}
