module Wasp.NpmDependencies
  ( NpmDependencies (..),
    empty,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import NpmDependency

data NpmDependencies = NpmDependencies
  { _dependencies :: ![NpmDependency]
  }
  deriving (Show, Eq)

empty :: NpmDependencies
empty = NpmDependencies {_dependencies = []}

instance ToJSON NpmDependencies where
  toJSON deps =
    object
      [ "dependencies" .= _dependencies deps
      ]
