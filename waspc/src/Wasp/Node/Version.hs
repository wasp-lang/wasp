module Wasp.Node.Version
  ( VersionCheckResult (..),
    oldestWaspSupportedNpmVersion,
    oldestWaspSupportedNodeVersion,
    isRangeInWaspSupportedRange,
    getAndCheckUserNodeAndNpmVersion,
  )
where

import Data.Conduit.Process.Typed (ExitCode (..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.Process as P
import qualified Text.Parsec as P
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.SemanticVersion.Version as SV
import qualified Wasp.SemanticVersion.VersionBound as SV
import Wasp.Util (indent)

-- | Wasp supports any node version equal or greater to this version.
-- | We usually keep this one equal to the latest LTS.
-- NOTE: If you change this value, make sure to also update it on some other places:
--   - /.github/workflows/ci.yaml -> actions/setup-node -> node-version
--   - /waspc/.nvmrc
--   - /web/docs/introduction/getting-started.md -> "Requirements" section.
oldestWaspSupportedNodeVersion :: SV.Version
oldestWaspSupportedNodeVersion = SV.Version 20 0 0

oldestWaspSupportedNpmVersion :: SV.Version
oldestWaspSupportedNpmVersion = SV.Version 9 6 0

isRangeInWaspSupportedRange :: SV.Range -> Bool
isRangeInWaspSupportedRange range =
  SV.versionBounds range `SV.isSubintervalOf` waspVersionInterval
  where
    waspVersionInterval = SV.versionBounds $ SV.backwardsCompatibleWith oldestWaspSupportedNodeVersion

data VersionCheckResult
  = VersionCheckFail !ErrorMessage
  | VersionCheckSuccess

type ErrorMessage = String

-- | Gets the user's installed Node and NPM version, if any are installed,
-- and checks that they meet Wasp's requirements.
getAndCheckUserNodeAndNpmVersion :: IO VersionCheckResult
getAndCheckUserNodeAndNpmVersion = do
  nodeVersion <- getAndCheckUserNodeVersion
  npmVersion <- getAndCheckUserNpmVersion
  return $ case (nodeVersion, npmVersion) of
    (VersionCheckSuccess, VersionCheckSuccess) -> VersionCheckSuccess
    (VersionCheckFail nodeError, _) -> VersionCheckFail nodeError
    (_, VersionCheckFail npmError) -> VersionCheckFail npmError

getAndCheckUserNodeVersion :: IO VersionCheckResult
getAndCheckUserNodeVersion = getAndCheckUserToolVersion "node" oldestWaspSupportedNodeVersion

getAndCheckUserNpmVersion :: IO VersionCheckResult
getAndCheckUserNpmVersion = getAndCheckUserToolVersion "npm" oldestWaspSupportedNpmVersion

getAndCheckUserToolVersion :: String -> SV.Version -> IO VersionCheckResult
getAndCheckUserToolVersion commandName oldestSupportedToolVersion = checkToolVersion getToolVersion
  where
    checkToolVersion = checkInstalledVersionIsNewerThanOldestSupported commandName oldestSupportedToolVersion
    getToolVersion = parseVersionFromCommandOutput commandName <$> runCommandWithVersionFlag commandName

checkInstalledVersionIsNewerThanOldestSupported :: String -> SV.Version -> IO (Either ErrorMessage SV.Version) -> IO VersionCheckResult
checkInstalledVersionIsNewerThanOldestSupported commandName oldestSupportedVersion getInstalledVersion = do
  getInstalledVersion >>= \case
    Left errorMsg -> return $ VersionCheckFail errorMsg
    Right userVersion ->
      return $
        if SV.isVersionInRange userVersion $ SV.Range [SV.gte oldestSupportedVersion]
          then VersionCheckSuccess
          else VersionCheckFail $ versionMismatchErrorMessage userVersion
  where
    versionMismatchErrorMessage :: SV.Version -> ErrorMessage
    versionMismatchErrorMessage userVersion =
      unlines
        [ "Your " ++ commandName ++ " version does not meet Wasp's requirements! You are running " ++ commandName ++ " " <> show userVersion <> ".",
          "Wasp requires " ++ commandName ++ " version " <> show oldestSupportedVersion <> " or higher."
        ]

runCommandWithVersionFlag :: String -> IO (Either ErrorMessage (ExitCode, String, String))
runCommandWithVersionFlag commandName = do
  catchIOError
    (Right <$> P.readProcessWithExitCode commandName args "")
    ( \e ->
        return . Left $
          if isDoesNotExistError e
            then commandNotFoundErrorMessage
            else unkownErrorErrorMessage e
    )
  where
    args = ["--version"]
    commandWithArgs = unwords $ commandName : args

    commandNotFoundErrorMessage :: ErrorMessage
    commandNotFoundErrorMessage = "`" ++ commandWithArgs ++ "` command not found!"

    unkownErrorErrorMessage :: IOError -> ErrorMessage
    unkownErrorErrorMessage err =
      unlines
        [ "An error occurred while trying to run `" ++ commandWithArgs ++ ":",
          indent 2 $ show err
        ]

parseVersionFromCommandOutput :: String -> Either ErrorMessage (ExitCode, String, String) -> Either ErrorMessage SV.Version
parseVersionFromCommandOutput commandName result =
  case result of
    Left processError ->
      Left $ failedToRunCommandErrorMessage processError
    Right (ExitFailure exitCode, _, stderr) ->
      Left $ commandFailedWithExitCodeErrorMessage exitCode stderr
    Right (ExitSuccess, stdout, _) ->
      case parseVersionFromOutput stdout of
        Left parseError -> Left $ failedToParseVersionErrorMessage parseError
        Right version -> Right version
  where
    args = ["--version"]
    commandWithArgs = unwords $ commandName : args

    failedToRunCommandErrorMessage :: String -> ErrorMessage
    failedToRunCommandErrorMessage processError =
      unlines
        [ "Running `" ++ commandWithArgs ++ "` failed.",
          indent 2 processError,
          "Make sure you have `" ++ commandName ++ "` installed and in your PATH."
        ]

    commandFailedWithExitCodeErrorMessage :: Int -> String -> ErrorMessage
    commandFailedWithExitCodeErrorMessage exitCode commandError =
      unlines
        [ "Running `" ++ commandWithArgs ++ "` failed (exit code " ++ show exitCode ++ "):",
          indent 2 commandError
        ]

    parseVersionFromOutput :: String -> Either P.ParseError SV.Version
    parseVersionFromOutput = P.parse versionParser ""
      where
        versionParser = skipAnyCharTillMatch SV.versionParser
        skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p

    failedToParseVersionErrorMessage :: P.ParseError -> ErrorMessage
    failedToParseVersionErrorMessage parseError =
      unlines
        [ "Wasp failed to parse `" ++ commandName ++ "` version provided by `" ++ commandWithArgs ++ ".",
          show parseError,
          "This is most likely a bug in Wasp, please file an issue."
        ]
