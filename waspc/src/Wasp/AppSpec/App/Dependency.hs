{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Dependency
  ( Dependency (..),
    fromList,
    make,
  )
where

import Data.Data (Data)

data Dependency = Dependency
  { name :: String,
    version :: String
  }
  deriving (Show, Eq, Data)

instance Ord Dependency where
  a <= b = name a <= name b

fromList :: [(String, String)] -> [Dependency]
fromList = map make

make :: (String, String) -> Dependency
make (n, v) = Dependency {name = n, version = v}