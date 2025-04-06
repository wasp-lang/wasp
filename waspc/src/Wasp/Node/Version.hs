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

checkUserNodeAndNpmMeetWaspRequirements :: IO VersionCheckResult
checkUserNodeAndNpmMeetWaspRequirements = do
  nodeResult <- checkUserNodeVersion
  npmResult <- checkUserNpmVersion
  return $ case (nodeResult, npmResult) of
    (VersionCheckSuccess, VersionCheckSuccess) -> VersionCheckSuccess
    (VersionCheckFail nodeError, _) -> VersionCheckFail nodeError
    (_, VersionCheckFail npmError) -> VersionCheckFail npmError

checkUserNodeVersion :: IO VersionCheckResult
checkUserNodeVersion = checkUserToolVersion "node" ["--version"] oldestWaspSupportedNodeVersion

checkUserNpmVersion :: IO VersionCheckResult
checkUserNpmVersion = checkUserToolVersion "npm" ["--version"] oldestWaspSupportedNpmVersion

checkUserToolVersion :: String -> [String] -> SV.Version -> IO VersionCheckResult
checkUserToolVersion commandName commandArgs oldestSupportedToolVersion = do
  runResult <- runToolCommand
  return $ case runResult
    >>= praseToolCommandOutput
    >>= parseVersionFromToolCommandOutput
    >>= checkUserToolVersionIsSupported of
    Right _ -> VersionCheckSuccess
    Left errorMsg -> VersionCheckFail errorMsg
  where
    runToolCommand = runCommand commandName commandArgs
    praseToolCommandOutput = parseCommandResult commandName commandArgs
    parseVersionFromToolCommandOutput = parseVersionFromCommandOutput commandName commandArgs
    checkUserToolVersionIsSupported = checkUserVersionIsSupported commandName oldestSupportedToolVersion

runCommand :: String -> [String] -> IO (Either ErrorMessage (ExitCode, String, String))
runCommand commandName commandArgs =
  catchIOError
    (Right <$> P.readProcessWithExitCode commandName commandArgs "")
    ( \e ->
        return . Left . runCommandeErrorMessageWrapper $
          if isDoesNotExistError e
            then commandNotFoundErrorMessage
            else unkownErrorErrorMessage e
    )
  where
    runCommandeErrorMessageWrapper processError =
      unlines
        [ "Running `" ++ prettyPrintCommand commandName commandArgs ++ "` failed.",
          indent 2 processError,
          "Make sure you have `" ++ commandName ++ "` installed and in your PATH."
        ]

    commandNotFoundErrorMessage = "`" ++ prettyPrintCommand commandName commandArgs ++ "` command not found!"

    unkownErrorErrorMessage err =
      unlines
        [ "An error occurred while trying to run `" ++ prettyPrintCommand commandName commandArgs ++ ":",
          indent 2 $ show err
        ]

parseCommandResult :: String -> [String] -> (ExitCode, String, String) -> Either ErrorMessage String
parseCommandResult commandName commandArgs commandResult = case commandResult of
  (ExitSuccess, stdout, _) -> Right stdout
  (ExitFailure exitCode, _, stderr) -> Left $ commandFailedWithExitCodeErrorMessage exitCode stderr
  where
    commandFailedWithExitCodeErrorMessage exitCode commandError =
      unlines
        [ "Running `" ++ prettyPrintCommand commandName commandArgs ++ "` failed (exit code " ++ show exitCode ++ "):",
          indent 2 commandError
        ]

parseVersionFromCommandOutput :: String -> [String] -> String -> Either ErrorMessage SV.Version
parseVersionFromCommandOutput commandName commandArgs commandOutput =
  case findAndParseVersion commandOutput of
    Left parseError -> Left $ failedToParseVersionErrorMessage parseError
    Right version -> Right version
  where
    failedToParseVersionErrorMessage parseError =
      unlines
        [ "Wasp failed to parse `" ++ commandName ++ "` version provided by `" ++ prettyPrintCommand commandName commandArgs ++ ".",
          show parseError,
          "This is most likely a bug in Wasp, please file an issue at https://github.com/wasp-lang/wasp/issues."
        ]

findAndParseVersion :: String -> Either P.ParseError SV.Version
findAndParseVersion = P.parse versionParser ""
  where
    versionParser = skipAnyCharTillMatch SV.versionParser
    skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p

checkUserVersionIsSupported :: String -> SV.Version -> SV.Version -> Either ErrorMessage ()
checkUserVersionIsSupported commandName oldestSupportedVersion userVersion =
  if SV.isVersionInRange userVersion $ SV.Range [SV.gte oldestSupportedVersion]
    then Right ()
    else Left $ versionMismatchErrorMessage userVersion
  where
    versionMismatchErrorMessage version =
      unlines
        [ "Your " ++ commandName ++ " version does not meet Wasp's requirements!",
          "You are running " ++ commandName ++ " " ++ show version ++ ".",
          "Wasp requires " ++ commandName ++ " version " ++ show oldestSupportedVersion ++ " or higher."
        ]

prettyPrintCommand :: String -> [String] -> String
prettyPrintCommand commandName commandArgs =
  unwords $ commandName : commandArgs
