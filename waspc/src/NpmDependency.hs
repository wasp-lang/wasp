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
printDep dep =
  Term.applyStyles [Term.Cyan] (_name dep)
    ++ "@"
    ++ Term.applyStyles [Term.Yellow] (_version dep)

instance ToJSON NpmDependency where
  toJSON npmDep =
    object
      [ "name" .= _name npmDep,
        "version" .= _version npmDep
      ]
