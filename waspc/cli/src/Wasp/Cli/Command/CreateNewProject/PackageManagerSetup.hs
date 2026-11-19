module Wasp.Cli.Command.CreateNewProject.PackageManagerSetup
  ( setUpProjectForPackageManager,

    -- * Exported for testing
    removeAllowScriptsField,
    makePnpmWorkspaceFileContent,
    useCommandsOfPackageManager,
  )
where

import Control.Monad (forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File', Path', relfile, (</>))
import qualified StrongPath as SP
import Wasp.Cli.DevWaspNpmPackages
  ( DevWaspNpmPackageTarballs,
    addDevWaspNpmPackageOverridesToPackageJsonFile,
    getDevWaspNpmPackageOverrides,
  )
import qualified Wasp.Cli.PnpmWorkspaceFile as PnpmWorkspaceFile
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Project.PackageManager
  ( PackageManager (..),
    getPackageManagerName,
    pnpmWorkspaceFileName,
  )
import qualified Wasp.Util.IO as IOUtil

-- | The starter templates are set up for npm. This turns what is specific to
-- npm in a new project into what the project's package manager uses.
--
-- With dev tarballs of the Wasp npm packages (see
-- "Wasp.Cli.DevWaspNpmPackages"), it also points the package manager at them.
setUpProjectForPackageManager :: PackageManager -> Maybe DevWaspNpmPackageTarballs -> Path' Abs (Dir WaspProjectDir) -> IO ()
setUpProjectForPackageManager packageManager maybeDevTarballs projectDir = case packageManager of
  Npm -> addDevOverridesToPackageJson
  Bun -> do
    -- Bun allows the install scripts of well-known packages on its own.
    _packagesAllowedToRunScripts <- removeNpmSettings
    -- Bun reads the overrides from package.json, like npm.
    addDevOverridesToPackageJson
    useCommandsOfPackageManagerInDocs
  Pnpm -> do
    packagesAllowedToRunScripts <- removeNpmSettings
    IOUtil.writeFile pnpmWorkspaceFile
      $ maybe id (PnpmWorkspaceFile.appendOverrides . getDevWaspNpmPackageOverrides) maybeDevTarballs
      $ makePnpmWorkspaceFileContent packagesAllowedToRunScripts
    useCommandsOfPackageManagerInDocs
  where
    addDevOverridesToPackageJson =
      forM_ maybeDevTarballs $ \devTarballs ->
        findUserPackageJsonFile projectDir
          >>= mapM_ (addDevWaspNpmPackageOverridesToPackageJsonFile devTarballs)

    -- Returns the names of the packages that npm was allowed to run install
    -- scripts of.
    removeNpmSettings :: IO [String]
    removeNpmSettings = do
      IOUtil.deleteFileIfExists npmSettingsFile
      findUserPackageJsonFile projectDir >>= \case
        Nothing -> return []
        Just packageJsonFile -> do
          packageJsonText <- IOUtil.readFileStrict packageJsonFile
          IOUtil.writeFileFromText packageJsonFile $ removeAllowScriptsField packageJsonText
          return $ getAllowScriptsPackageNames packageJsonText

    useCommandsOfPackageManagerInDocs =
      forM_ docFiles $ \docFile -> do
        doesDocFileExist <- IOUtil.doesFileExist docFile
        when doesDocFileExist $
          IOUtil.readFileStrict docFile >>= IOUtil.writeFileFromText docFile . useCommandsOfPackageManager packageManager

    npmSettingsFile :: Path' Abs File'
    npmSettingsFile = projectDir </> [relfile|.npmrc|]

    pnpmWorkspaceFile :: Path' Abs File'
    pnpmWorkspaceFile = projectDir </> fromMaybe (error "A file name is a relative path") (SP.parseRelFile pnpmWorkspaceFileName)

    docFiles :: [Path' Abs File']
    docFiles = (projectDir </>) <$> [[relfile|README.md|], [relfile|AGENTS.md|]]

allowScriptsFieldName :: String
allowScriptsFieldName = "allowScripts"

getAllowScriptsPackageNames :: T.Text -> [String]
getAllowScriptsPackageNames packageJsonText = fromMaybe [] $ do
  Aeson.Object packageJson <- Aeson.decodeStrictText packageJsonText
  Aeson.Object allowScripts <- KM.lookup (Key.fromString allowScriptsFieldName) packageJson
  return $ Key.toString <$> KM.keys allowScripts

-- | Removes npm's `allowScripts` field from the text of a package.json, and
-- leaves the rest of the text as it is. Writing parsed JSON back would
-- reorder the fields.
--
-- It expects the field the way the starter templates have it: a top-level
-- object, each line indented with two spaces. Otherwise it changes nothing.
removeAllowScriptsField :: T.Text -> T.Text
removeAllowScriptsField packageJsonText =
  if isValidJsonWithoutField textWithoutField then textWithoutField else packageJsonText
  where
    textWithoutField = T.unlines $ fromMaybe allLines $ do
      (linesBeforeField, _fieldStartLine : linesFromField) <- Just $ break (fieldStart `T.isPrefixOf`) allLines
      (_fieldBodyLines, fieldEndLine : linesAfterField) <- Just $ break (`elem` [fieldEnd, lastFieldEnd]) linesFromField
      return $
        if fieldEndLine == lastFieldEnd
          then dropTrailingCommaOfLastLine linesBeforeField ++ linesAfterField
          else linesBeforeField ++ linesAfterField

    allLines = T.lines packageJsonText

    fieldStart = T.pack $ "  " ++ show allowScriptsFieldName ++ ": {"
    fieldEnd = "  },"
    -- Without a comma, so the field is the last one of package.json.
    lastFieldEnd = "  }"

    -- The field before the last field ends with a comma, and JSON doesn't
    -- allow one once it is the last field itself.
    dropTrailingCommaOfLastLine [] = []
    dropTrailingCommaOfLastLine textLines = init textLines ++ [fromMaybe (last textLines) $ T.stripSuffix "," $ last textLines]

    isValidJsonWithoutField text = case Aeson.decodeStrictText text of
      Just (Aeson.Object json) -> not $ KM.member (Key.fromString allowScriptsFieldName) json
      _ -> False

-- | What `.npmrc` and the `allowScripts` of package.json say in the starter
-- templates, in pnpm's settings.
makePnpmWorkspaceFileContent :: [String] -> String
makePnpmWorkspaceFileContent packagesAllowedToRunScripts =
  unlines $
    [ "# Helps prevent supply chain attacks. In minutes.",
      "minimumReleaseAge: 10080",
      "# Wasp publishes its own packages with each release. Exempt them, so projects",
      "# can install a new Wasp release on the day it comes out.",
      "minimumReleaseAgeExclude:",
      "  - \"@wasp/*\"",
      "# The packages that may run scripts when pnpm installs them.",
      "allowBuilds:"
    ]
      ++ map (\packageName -> "  " ++ show packageName ++ ": true") packagesAllowedToRunScripts
      ++ [ "# pnpm installs the other packages without running their scripts, instead of",
           "# failing.",
           "strictDepBuilds: false"
         ]

-- | The docs of the starter templates name npm's commands.
useCommandsOfPackageManager :: PackageManager -> T.Text -> T.Text
useCommandsOfPackageManager packageManager =
  replaceAll placeholdersWithCommands . replaceAll npmCommandsWithPlaceholders
  where
    replaceAll replacements text = foldl (\text' (old, new) -> T.replace old new text') text replacements

    -- A command of pnpm contains the same command of npm, so replacing one
    -- command after another would replace inside of what's already replaced.
    npmCommandsWithPlaceholders = zip (map (T.pack . fst) commandReplacements) placeholders
    placeholdersWithCommands = zip placeholders (map (T.pack . snd) commandReplacements)
    placeholders = [T.pack $ "\0" ++ show index ++ "\0" | index <- [0 :: Int ..]]

    -- Longer commands first, since they start with the shorter ones.
    commandReplacements =
      [ -- Only npm needs `--` before the arguments of a script.
        (npm ++ " run wasp -- ", name ++ " run wasp "),
        (npm ++ " run ", name ++ " run "),
        (npm ++ " install", name ++ " install")
      ]

    npm = getPackageManagerName Npm
    name = getPackageManagerName packageManager
