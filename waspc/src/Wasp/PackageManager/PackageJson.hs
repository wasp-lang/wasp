module Wasp.PackageManager.PackageJson
  ( hasWorkspacesField,
    parseDevEnginesField,
    parsePackageManagerField,
  )
where

import Control.Monad ((>=>))
import Data.Aeson (Value (..), withObject, (.:))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Wasp.PackageManager.PackageManager (PackageManager, parsePackageManagerName)

-- | Reads Corepack's @"packageManager": "pnpm\@9.12.1+sha512..."@.
parsePackageManagerField :: Value -> Maybe PackageManager
parsePackageManagerField =
  parseMaybe (withObject "package.json" (.: "packageManager"))
    >=> parsePackageManagerName . takeWhile (/= '@')

-- | Reads @"devEngines": { "packageManager": { "name": "pnpm" } }@. The spec
-- also allows a list of alternatives, any of which the project accepts, so
-- this gives all of them in the order they are listed.
parseDevEnginesField :: Value -> [PackageManager]
parseDevEnginesField packageJson =
  mapMaybe (parseMaybe nameParser >=> parsePackageManagerName) alternatives
  where
    alternatives = fromMaybe [] $ parseMaybe devEnginesPackageManagerParser packageJson

    devEnginesPackageManagerParser :: Value -> Parser [Value]
    devEnginesPackageManagerParser =
      withObject "package.json" $
        (.: "devEngines")
          >=> withObject "devEngines" (.: "packageManager")
          >=> \case
            Array listedAlternatives -> return $ toList listedAlternatives
            single -> return [single]

    nameParser :: Value -> Parser String
    nameParser = withObject "packageManager" (.: "name")

-- | Whether the package.json declares an npm, Yarn or Bun workspace. That is
-- a list of globs, or an object like @{ "packages": [...] }@.
hasWorkspacesField :: Value -> Bool
hasWorkspacesField = \case
  Object packageJson -> case KeyMap.lookup "workspaces" packageJson of
    Just (Array _) -> True
    Just (Object _) -> True
    _ -> False
  _ -> False
