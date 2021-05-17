module NpmDependency
  ( NpmDependency (..),
    fromList,
    printDep,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Util.Terminal as Term

data NpmDependency = NpmDependency
  { _name :: !String,
    _version :: !String
  }
  deriving (Show, Eq)

fromList :: [(String, String)] -> [NpmDependency]
fromList = map (\(name, version) -> NpmDependency {_name = name, _version = version})

printDep :: NpmDependency -> String
printDep (NpmDependency {_name = name, _version = version}) = Term.applyStyles [Term.Cyan] name ++ "@" ++ Term.applyStyles [Term.Yellow] version

instance ToJSON NpmDependency where
  toJSON npmDep =
    object
      [ "name" .= _name npmDep,
        "version" .= _version npmDep
      ]
