module Wasp.Node.Version
  ( VersionCheckResult (..),
    getAndCheckUserNodeVersion,
    getAndCheckUserNPMVersion,
    oldestWaspSupportedNPMVersion,
    oldestWaspSupportedNodeVersion,
    isRangeInWaspSupportedRange,
    checkVersion,
  )
where

import Control.Arrow (left)
import Data.Conduit.Process.Typed (ExitCode (..))
import Data.Function ((&))
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

-- | Gets the user's installed node version, if any is installed, and checks that it
-- meets Wasp's version requirement.
getAndCheckUserNodeVersion :: IO VersionCheckResult
getAndCheckUserNodeVersion = checkVersion "node" oldestWaspSupportedNodeVersion parseVersionOutput

-- | Gets the user's installed NPM version, if any is installed, and checks that it
-- meets Wasp's version requirement.
getAndCheckUserNPMVersion :: IO VersionCheckResult
getAndCheckUserNPMVersion = checkVersion "npm" oldestWaspSupportedNPMVersion parseVersionOutput

-- | Base function to check if the user's installed version meets Wasp's version requirement.
checkVersion :: String -> SV.Version -> (String -> Either P.ParseError SV.Version) -> IO VersionCheckResult
checkVersion command oldestSupportedVersion parseVersion = do
  result <- getUserVersion command parseVersion
  return $ case result of
    Left errorMsg -> VersionCheckFail errorMsg
    Right userVersion ->
      if SV.isVersionInRange userVersion $ SV.Range [SV.gte oldestSupportedVersion]
        then VersionCheckSuccess
        else VersionCheckFail $ makeVersionMismatchMessage command userVersion oldestSupportedVersion

-- | Base function to get the installed version of a command, if any is installed, and returns it.
getUserVersion :: String -> (String -> Either P.ParseError SV.Version) -> IO (Either ErrorMessage SV.Version)
getUserVersion command parseVersion = do
  result <-
    (Right <$> P.readProcessWithExitCode command ["--version"] "")
      `catchIOError` ( \e ->
                         if isDoesNotExistError e
                           then return $ Left $ commandNotFoundMessage command
                           else return $ Left $ makeUnknownErrorMessage command e
                     )
  return $ case result of
    Left procErr ->
      Left
        ( unlines
            [ "Running `" ++ command ++ " --version` failed.",
              indent 2 procErr,
              "Make sure you have `" ++ command ++ "` installed and in your PATH."
            ]
        )
    Right (ExitFailure code, _, stderr) ->
      Left
        ( unlines
            [ "Running `" ++ command ++ " --version` failed (exit code " ++ show code ++ "):",
              indent 2 stderr
            ]
        )
    Right (ExitSuccess, stdout, _) ->
      parseVersion stdout
        & left
          ( \e ->
              "Wasp failed to parse `" ++ command ++ "` version provided by `" ++ command ++ " --version`.\n"
                <> (show e <> "\n")
                <> "This is most likely a bug in Wasp, please file an issue."
          )

-- | Extracts Node/NPM version from the output of `--version` flag.
parseVersionOutput :: String -> Either P.ParseError SV.Version
parseVersionOutput = P.parse versionParser ""
  where
    versionParser = skipAnyCharTillMatch SV.versionParser
    skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p

commandNotFoundMessage :: String -> String
commandNotFoundMessage command = "`" ++ command ++ "` command not found!"

makeUnknownErrorMessage :: String -> IOError -> String
makeUnknownErrorMessage command err =
  unlines
    [ "An unknown error occurred while trying to run `" ++ command ++ " --version`:",
      indent 2 $ show err
    ]

makeVersionMismatchMessage :: String -> SV.Version -> SV.Version -> String
makeVersionMismatchMessage command userVersion requiredVersion =
  unlines
    [ "Your " ++ command ++ " version does not meet Wasp's requirements! You are running " ++ command ++ " " <> show userVersion <> ".",
      "Wasp requires " ++ command ++ " version " <> show requiredVersion <> " or higher."
    ]
