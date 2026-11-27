module Wasp.PackageManager.Detection.PackageJson
  ( hasWorkspacesField,
    parseDevEnginesField,
    parsePackageManagerField,
    readPackageJsonIfValid,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Extra (ifM)
import Data.Aeson (Value (..), withObject, (.:))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import StrongPath (Abs, Dir, Path', relfile, (</>))
import Wasp.PackageManager.Detection.Common (PackageRootDir)
import Wasp.PackageManager.PackageManager (PackageManager, parsePackageManagerName)
import Wasp.Util.IO (doesFileExist)
import Wasp.Util.Json (readJsonFile)

readPackageJsonIfValid :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Value)
readPackageJsonIfValid dir =
  ifM (doesFileExist packageJsonPath) (readJsonFile packageJsonPath) (return Nothing)
  where
    packageJsonPath = dir </> [relfile|package.json|]

-- | Reads Corepack's @"packageManager": "pnpm\@9.12.1+sha512..."@.
parsePackageManagerField :: Value -> Maybe PackageManager
parsePackageManagerField =
  parseMaybe (withObject "package.json" (.: "packageManager"))
    >=> parsePackageManagerName . takeWhile (/= '@')

-- | Reads @"devEngines": { "packageManager": { "name": "pnpm" } }@. The spec
-- also allows a list of alternatives. We take the first one as the project's
-- preference and ignore the rest.
parseDevEnginesField :: Value -> Maybe PackageManager
parseDevEnginesField =
  parseMaybe preferredPackageManagerNameParser >=> parsePackageManagerName
  where
    preferredPackageManagerNameParser :: Value -> Parser String
    preferredPackageManagerNameParser =
      withObject "package.json" $
        (.: "devEngines")
          >=> withObject "devEngines" (.: "packageManager")
          >=> \case
            Array alternatives -> maybe (fail "no alternatives") nameParser (listToMaybe $ toList alternatives)
            single -> nameParser single

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
