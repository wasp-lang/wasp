{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Dependency
  ( Dependency (..),
    fromList,
    create,
  )
where

import Data.Data (Data)

data Dependency = Dependency
  { name :: String,
    version :: String
  }
  deriving (Show, Eq, Data)

fromList :: [(String, String)] -> [Dependency]
fromList = map create

create :: (String, String) -> Dependency
create (n, v) = Dependency {name = n, version = v}