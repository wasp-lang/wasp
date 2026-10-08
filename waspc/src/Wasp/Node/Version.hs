module Wasp.Node.Version
  ( VersionCheckResult,
    oldestWaspSupportedNpmVersion,
    oldestWaspSupportedNodeVersion,
    nodeTypesVersionRangeMatchingNodeMajor,
    isRangeInWaspSupportedRange,
    checkUserNodeAndNpmMeetWaspRequirements,
    getUserNodeVersion,
    getUserNpmVersion,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Conduit.Process.Typed (ExitCode (..))
import Data.Functor ((<&>))
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Process (readProcessWithExitCode)
import Wasp.Node.Internal (parseVersionFromCommandOutput)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (indent)

-- | Wasp supports any node version equal or greater to this version.
-- | We usually keep this one equal to the latest LTS.
-- NOTE: Don't change Wasp's lowest supported Node version without updating it
-- in all required places. Check /mise.toml for the full list.
oldestWaspSupportedNodeVersion :: SV.Version
oldestWaspSupportedNodeVersion = SV.Version 24 14 1

oldestWaspSupportedNpmVersion :: SV.Version
oldestWaspSupportedNpmVersion = SV.Version 11 11 0

nodeTypesVersionRangeMatchingNodeMajor :: SV.Version -> SV.Range
nodeTypesVersionRangeMatchingNodeMajor nodeVersion =
  SV.backwardsCompatibleWith $ SV.Version (SV.major nodeVersion) 0 0

isRangeInWaspSupportedRange :: SV.Range -> Bool
isRangeInWaspSupportedRange range =
  SV.versionBounds range `SV.isSubintervalOf` waspVersionInterval
  where
    waspVersionInterval = SV.versionBounds $ SV.backwardsCompatibleWith oldestWaspSupportedNodeVersion

type VersionCheckResult = Either ErrorMessage ()

type ErrorMessage = String

checkUserNodeAndNpmMeetWaspRequirements :: IO VersionCheckResult
checkUserNodeAndNpmMeetWaspRequirements =
  runExceptT $
    mapM_
      ExceptT
      [ checkUserToolVersion "node" ["--version"] oldestWaspSupportedNodeVersion,
        checkUserToolVersion "npm" ["--version"] oldestWaspSupportedNpmVersion
      ]

checkUserToolVersion :: String -> [String] -> SV.Version -> IO VersionCheckResult
checkUserToolVersion commandName commandArgs oldestSupportedToolVersion =
  getToolVersionFromCommandOutput commandName commandArgs
    <&> (>>= assertVersionIsSupported)
  where
    assertVersionIsSupported userVersion
      | userVersion >= oldestSupportedToolVersion = Right ()
      | otherwise = Left $ makeVersionMismatchErrorMessage userVersion

    makeVersionMismatchErrorMessage version =
      unlines
        [ "Your " ++ commandName ++ " version does not meet Wasp's requirements!",
          "You are running " ++ commandName ++ " " ++ show version ++ ".",
          "Wasp requires " ++ commandName ++ " version " ++ show oldestSupportedToolVersion ++ " or higher."
        ]

getUserNodeVersion :: IO (Either ErrorMessage SV.Version)
getUserNodeVersion = getToolVersionFromCommandOutput "node" ["--version"]

getUserNpmVersion :: IO (Either ErrorMessage SV.Version)
getUserNpmVersion = getToolVersionFromCommandOutput "npm" ["--version"]

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
