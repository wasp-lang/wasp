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
import Data.Foldable (asum, toList)
import Wasp.PackageManager.PackageManager (PackageManager, parsePackageManagerName)

-- | Reads Corepack's @"packageManager": "pnpm\@9.12.1+sha512..."@.
parsePackageManagerField :: Value -> Maybe PackageManager
parsePackageManagerField =
  parseMaybe (withObject "package.json" (.: "packageManager"))
    >=> parsePackageManagerName . takeWhile (/= '@')

-- | Reads @"devEngines": { "packageManager": { "name": "pnpm" } }@. The spec
-- also allows a list of alternatives, in which case the first one Wasp
-- supports wins.
parseDevEnginesField :: Value -> Maybe PackageManager
parseDevEnginesField packageJson = do
  alternatives <- parseMaybe devEnginesPackageManagerParser packageJson
  asum $ map (parseMaybe nameParser >=> parsePackageManagerName) alternatives
  where
    devEnginesPackageManagerParser :: Value -> Parser [Value]
    devEnginesPackageManagerParser =
      withObject "package.json" $
        (.: "devEngines")
          >=> withObject "devEngines" (.: "packageManager")
          >=> \case
            Array alternatives -> return $ toList alternatives
            single -> return [single]

    nameParser :: Value -> Parser String
    nameParser = withObject "packageManager" (.: "name")

-- | Whether the package.json declares an npm or Bun workspace. That is a list
-- of globs, or an object like @{ "packages": [...] }@.
hasWorkspacesField :: Value -> Bool
hasWorkspacesField = \case
  Object packageJson -> case KeyMap.lookup "workspaces" packageJson of
    Just (Array _) -> True
    Just (Object _) -> True
    _ -> False
  _ -> False
