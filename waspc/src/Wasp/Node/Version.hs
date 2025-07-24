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
import System.Process (readProcessWithExitCode)
import Wasp.Node.Internal (parseVersionFromCommandOutput)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.SemanticVersion.VersionBound as SV
import Wasp.Util (indent)

-- | Wasp supports any node version equal or greater to this version.
-- | We usually keep this one equal to the latest LTS.
-- NOTE: If you change this value, make sure to also update it on some other places:
--   - /.github/workflows/ci.yaml -> actions/setup-node -> node-version
--   - /waspc/.nvmrc
--   - /web/docs/introduction/getting-started.md -> "Requirements" section.
oldestWaspSupportedNodeVersion :: SV.Version
oldestWaspSupportedNodeVersion = SV.Version 22 12 0

oldestWaspSupportedNpmVersion :: SV.Version
oldestWaspSupportedNpmVersion = SV.Version 10 9 0

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
  where
    checkUserNodeVersion = checkUserToolVersion "node" ["--version"] oldestWaspSupportedNodeVersion
    checkUserNpmVersion = checkUserToolVersion "npm" ["--version"] oldestWaspSupportedNpmVersion

checkUserToolVersion :: String -> [String] -> SV.Version -> IO VersionCheckResult
checkUserToolVersion commandName commandArgs oldestSupportedToolVersion = do
  userVersionOrError <- getToolVersionFromCommandOutput commandName commandArgs
  return $ case userVersionOrError of
    Left errorMsg -> VersionCheckFail errorMsg
    Right userVersion
      | userVersion >= oldestSupportedToolVersion -> VersionCheckSuccess
      | otherwise -> VersionCheckFail $ makeVersionMismatchErrorMessage userVersion
  where
    makeVersionMismatchErrorMessage version =
      unlines
        [ "Your " ++ commandName ++ " version does not meet Wasp's requirements!",
          "You are running " ++ commandName ++ " " ++ show version ++ ".",
          "Wasp requires " ++ commandName ++ " version " ++ show oldestSupportedToolVersion ++ " or higher."
        ]

getToolVersionFromCommandOutput :: String -> [String] -> IO (Either ErrorMessage SV.Version)
getToolVersionFromCommandOutput commandName commandArgs = do
  commandOutput <- readCommandOutput commandName commandArgs
  return $ commandOutput >>= parseVersionFromCommandOutput

readCommandOutput :: String -> [String] -> IO (Either ErrorMessage String)
readCommandOutput commandName commandArgs = do
  commandResult <-
    catchIOError
      (Right <$> readProcessWithExitCode commandName commandArgs "")
      (return . Left . wrapCommandIOErrorMessage . makeIOErrorMessage)
  return $ case commandResult of
    Left procErr -> Left procErr
    Right (ExitFailure exitCode, _, stderr) -> Left $ wrapCommandExitCodeErrorMessage exitCode stderr
    Right (ExitSuccess, stdout, _) -> Right stdout
  where
    makeIOErrorMessage ioErr
      | isDoesNotExistError ioErr = "`" ++ fullCommand ++ "` command not found!"
      | otherwise = show ioErr

    wrapCommandIOErrorMessage innerErr =
      unlines
        [ "Running `" ++ fullCommand ++ "` failed.",
          indent 2 innerErr,
          "Make sure you have `" ++ commandName ++ "` installed and in your PATH."
        ]

    wrapCommandExitCodeErrorMessage exitCode commandErr =
      unlines
        [ "Running `" ++ fullCommand ++ "` failed (exit code " ++ show exitCode ++ "):",
          indent 2 commandErr
        ]

    fullCommand = unwords $ commandName : commandArgs
