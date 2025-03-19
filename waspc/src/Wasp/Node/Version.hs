module Wasp.Node.Version
  ( VersionCheckResult (..),
    oldestWaspSupportedNPMVersion,
    oldestWaspSupportedNodeVersion,
    isRangeInWaspSupportedRange,
    getAndCheckNodeAndNpmVersion,
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

oldestWaspSupportedNPMVersion :: SV.Version
oldestWaspSupportedNPMVersion = SV.Version 9 6 0

isRangeInWaspSupportedRange :: SV.Range -> Bool
isRangeInWaspSupportedRange range =
  SV.versionBounds range `SV.isSubintervalOf` waspVersionInterval
  where
    waspVersionInterval = SV.versionBounds $ SV.backwardsCompatibleWith oldestWaspSupportedNodeVersion

data VersionCheckResult
  = VersionCheckFail !ErrorMessage
  | VersionCheckSuccess

type ErrorMessage = String

getAndCheckNodeAndNpmVersion :: IO VersionCheckResult
getAndCheckNodeAndNpmVersion = do
  nodeVersion <- getAndCheckUserNodeVersion
  npmVersion <- getAndCheckUserNPMVersion
  return $ case (nodeVersion, npmVersion) of
    (VersionCheckSuccess, VersionCheckSuccess) -> VersionCheckSuccess
    (VersionCheckFail nodeError, VersionCheckFail npmError) -> VersionCheckFail $ nodeError ++ "\n" ++ npmError
    (VersionCheckFail nodeError, _) -> VersionCheckFail nodeError
    (_, VersionCheckFail npmError) -> VersionCheckFail npmError

getAndCheckUserNodeVersion :: IO VersionCheckResult
getAndCheckUserNodeVersion = checkNodeVersion getNodeVersion
  where
    checkNodeVersion = checkInstalledVersionNewerThanOldestSupported "node" oldestWaspSupportedNodeVersion
    getNodeVersion = parseVersionFromCommandOutput "node" ["--version"]

getAndCheckUserNPMVersion :: IO VersionCheckResult
getAndCheckUserNPMVersion = checkNPMVersion getNPMVersion
  where
    checkNPMVersion = checkInstalledVersionNewerThanOldestSupported "npm" oldestWaspSupportedNPMVersion
    getNPMVersion = parseVersionFromCommandOutput "npm" ["--version"]

checkInstalledVersionNewerThanOldestSupported :: String -> SV.Version -> IO (Either ErrorMessage SV.Version) -> IO VersionCheckResult
checkInstalledVersionNewerThanOldestSupported command oldestSupportedVersion getInstalledVersion = do
  result <- getInstalledVersion
  return $ case result of
    Left errorMsg -> VersionCheckFail errorMsg
    Right userVersion ->
      if SV.isVersionInRange userVersion $ SV.Range [SV.gte oldestSupportedVersion]
        then VersionCheckSuccess
        else VersionCheckFail $ versionMismatchErrorMessage userVersion
  where
    versionMismatchErrorMessage :: SV.Version -> ErrorMessage
    versionMismatchErrorMessage userVersion =
      unlines
        [ "Your " ++ command ++ " version does not meet Wasp's requirements! You are running " ++ command ++ " " <> show userVersion <> ".",
          "Wasp requires " ++ command ++ " version " <> show oldestSupportedVersion <> " or higher."
        ]

parseVersionFromCommandOutput :: String -> [String] -> IO (Either ErrorMessage SV.Version)
parseVersionFromCommandOutput command args = do
  result <-
    catchIOError
      (Right <$> P.readProcessWithExitCode command args "")
      ( \e ->
          if isDoesNotExistError e
            then return $ Left commandNotFoundErrorMessage
            else return $ Left $ unkownErrorErrorMessage e
      )
  return $ case result of
    Left processError ->
      Left $ runningCommandFailedErrorMessage processError
    Right (ExitFailure exitCode, _, stderr) ->
      Left $ commandFailedErrorMessage exitCode stderr
    Right (ExitSuccess, stdout, _) ->
      case parseVersionFromOutput stdout of
        Left parseError -> Left $ parseVersionFailedErrorMessage parseError
        Right version -> Right version
  where
    commandWithArgs = unwords $ command : args

    commandNotFoundErrorMessage :: ErrorMessage
    commandNotFoundErrorMessage = "`" ++ commandWithArgs ++ "` command not found!"

    unkownErrorErrorMessage :: IOError -> ErrorMessage
    unkownErrorErrorMessage err =
      unlines
        [ "An unknown error occurred while trying to run `" ++ commandWithArgs ++ ":",
          indent 2 $ show err
        ]

    runningCommandFailedErrorMessage :: String -> ErrorMessage
    runningCommandFailedErrorMessage processError =
      unlines
        [ "Running `" ++ commandWithArgs ++ "` failed.",
          indent 2 processError,
          "Make sure you have `" ++ command ++ "` installed and in your PATH."
        ]

    commandFailedErrorMessage :: Int -> String -> ErrorMessage
    commandFailedErrorMessage exitCode commandError =
      unlines
        [ "Running `" ++ commandWithArgs ++ "` failed (exit code " ++ show exitCode ++ "):",
          indent 2 commandError
        ]

    parseVersionFailedErrorMessage :: P.ParseError -> ErrorMessage
    parseVersionFailedErrorMessage parseError =
      unlines
        [ "Wasp failed to parse `" ++ command ++ "` version provided by `" ++ commandWithArgs ++ ".",
          show parseError,
          "This is most likely a bug in Wasp, please file an issue."
        ]

parseVersionFromOutput :: String -> Either P.ParseError SV.Version
parseVersionFromOutput = P.parse versionParser ""
  where
    versionParser = skipAnyCharTillMatch SV.versionParser
    skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p
