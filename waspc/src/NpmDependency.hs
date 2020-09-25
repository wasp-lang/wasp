module NpmDependency
    ( NpmDependency (..)
    , fromList
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))


data NpmDependency = NpmDependency
    { _name    :: !String
    , _version :: !String }
  deriving (Show, Eq)

fromList :: [(String, String)] -> [NpmDependency]
fromList = map (\(name, version) -> NpmDependency { _name = name, _version = version })

instance ToJSON NpmDependency where
    toJSON npmDep = object
        [ "name" .= _name npmDep
        , "version" .= _version npmDep
        ]
