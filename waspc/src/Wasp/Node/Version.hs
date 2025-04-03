module Wasp.Node.Version
  ( VersionCheckResult (..),
    oldestWaspSupportedNpmVersion,
    oldestWaspSupportedNodeVersion,
    isRangeInWaspSupportedRange,
    checkUserNodeAndNpmMeetWaspRequirements,
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

type Command = (String, [String])

checkUserNodeAndNpmMeetWaspRequirements :: IO VersionCheckResult
checkUserNodeAndNpmMeetWaspRequirements = do
  nodeResult <- checkUserNodeVersion
  npmResult <- checkUserNpmVersion
  return $ case (nodeResult, npmResult) of
    (VersionCheckSuccess, VersionCheckSuccess) -> VersionCheckSuccess
    (VersionCheckFail nodeError, _) -> VersionCheckFail nodeError
    (_, VersionCheckFail npmError) -> VersionCheckFail npmError

checkUserNodeVersion :: IO VersionCheckResult
checkUserNodeVersion = checkUserToolVersion ("node", ["--version"]) oldestWaspSupportedNodeVersion

checkUserNpmVersion :: IO VersionCheckResult
checkUserNpmVersion = checkUserToolVersion ("npm", ["--version"]) oldestWaspSupportedNpmVersion

checkUserToolVersion :: Command -> SV.Version -> IO VersionCheckResult
checkUserToolVersion command@(commandName, _) oldestSupportedToolVersion =
  runToolCommand >>= \case
    Left commandErr -> return $ VersionCheckFail $ failedToRunCommandErrorMessage commandErr
    Right commandResult -> return $ case parseVersionFromToolCommandOutput commandResult of
      Left errorMsg -> VersionCheckFail errorMsg
      Right version -> checkUserToolVersionIsSupported version
  where
    runToolCommand = runCommand command
    parseVersionFromToolCommandOutput = parseVersionFromCommandOutput command
    checkUserToolVersionIsSupported = checkUserVersionIsSupported commandName oldestSupportedToolVersion

    failedToRunCommandErrorMessage processError =
      unlines
        [ "Running `" ++ showFullCommand command ++ "` failed.",
          indent 2 processError,
          "Make sure you have `" ++ commandName ++ "` installed and in your PATH."
        ]

runCommand :: Command -> IO (Either ErrorMessage (ExitCode, String, String))
runCommand command@(commandName, commandArgs) =
  catchIOError
    (Right <$> P.readProcessWithExitCode commandName commandArgs "")
    ( \e ->
        return . Left $
          if isDoesNotExistError e
            then commandNotFoundErrorMessage
            else unkownErrorErrorMessage e
    )
  where
    commandNotFoundErrorMessage = "`" ++ showFullCommand command ++ "` command not found!"

    unkownErrorErrorMessage err =
      unlines
        [ "An error occurred while trying to run `" ++ showFullCommand command ++ ":",
          indent 2 $ show err
        ]

parseVersionFromCommandOutput :: Command -> (ExitCode, String, String) -> Either ErrorMessage SV.Version
parseVersionFromCommandOutput command@(commandName, _) commandResult =
  case commandResult of
    (ExitFailure exitCode, _, stderr) ->
      Left $ commandFailedWithExitCodeErrorMessage exitCode stderr
    (ExitSuccess, stdout, _) ->
      case findAndParseVersion stdout of
        Left parseError -> Left $ failedToParseVersionErrorMessage parseError
        Right version -> Right version
  where
    commandFailedWithExitCodeErrorMessage exitCode commandError =
      unlines
        [ "Running `" ++ showFullCommand command ++ "` failed (exit code " ++ show exitCode ++ "):",
          indent 2 commandError
        ]

    failedToParseVersionErrorMessage parseError =
      unlines
        [ "Wasp failed to parse `" ++ commandName ++ "` version provided by `" ++ showFullCommand command ++ ".",
          show parseError,
          "This is most likely a bug in Wasp, please file an issue at https://github.com/wasp-lang/wasp/issues."
        ]

findAndParseVersion :: String -> Either P.ParseError SV.Version
findAndParseVersion = P.parse versionParser ""
  where
    versionParser = skipAnyCharTillMatch SV.versionParser
    skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p

checkUserVersionIsSupported :: String -> SV.Version -> SV.Version -> VersionCheckResult
checkUserVersionIsSupported commandName oldestSupportedVersion userVersion =
  if SV.isVersionInRange userVersion $ SV.Range [SV.gte oldestSupportedVersion]
    then VersionCheckSuccess
    else VersionCheckFail $ versionMismatchErrorMessage userVersion
  where
    versionMismatchErrorMessage version =
      unlines
        [ "Your " ++ commandName ++ " version does not meet Wasp's requirements!",
          "You are running " ++ commandName ++ " " ++ show version ++ ".",
          "Wasp requires " ++ commandName ++ " version " ++ show oldestSupportedVersion ++ " or higher."
        ]

showFullCommand :: Command -> String
showFullCommand (commandName, commandArgs) =
  unwords $ commandName : commandArgs
