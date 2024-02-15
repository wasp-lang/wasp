module Wasp.Node.Version
  ( getAndCheckUserNodeVersion,
    VersionCheckResult (..),
    oldestWaspSupportedNodeVersion,
    parseNodeVersionOutput,
    isRangeInWaspSupportedRange,
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
oldestWaspSupportedNodeVersion = SV.Version 18 18 0

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
getAndCheckUserNodeVersion =
  getUserNodeVersion >>= \case
    Left errorMsg -> return $ VersionCheckFail errorMsg
    Right userNodeVersion ->
      return $
        if SV.isVersionInRange userNodeVersion $ SV.Range [SV.gte oldestWaspSupportedNodeVersion]
          then VersionCheckSuccess
          else VersionCheckFail $ makeNodeVersionMismatchMessage userNodeVersion

makeNodeVersionMismatchMessage :: SV.Version -> String
makeNodeVersionMismatchMessage nodeVersion =
  unlines
    [ "Your Node version does not meet Wasp's requirements! You are running Node " <> show nodeVersion <> ".",
      "Wasp requires Node version " <> show oldestWaspSupportedNodeVersion <> " or higher."
    ]

-- | Gets the installed node version, if any is installed, and returns it.
--
-- Returns a string representing the error condition if node's version could
-- not be found.
getUserNodeVersion :: IO (Either ErrorMessage SV.Version)
getUserNodeVersion = do
  -- Node result is one of:
  -- 1. @Left processError@, when an error occurs trying to run the process
  -- 2. @Right (ExitCode, stdout, stderr)@, when the node process runs and terminates
  nodeResult <-
    (Right <$> P.readProcessWithExitCode "node" ["--version"] "")
      `catchIOError` ( \e ->
                         if isDoesNotExistError e
                           then return $ Left nodeNotFoundMessage
                           else return $ Left $ makeNodeUnknownErrorMessage e
                     )
  return $ case nodeResult of
    Left procErr ->
      Left
        ( unlines
            [ "Running `node --version` failed.",
              indent 2 procErr,
              "Make sure you have `node` installed and in your PATH."
            ]
        )
    Right (ExitFailure code, _, stderr) ->
      Left
        ( unlines
            [ "Running `node --version` failed (exit code " ++ show code ++ "):",
              indent 2 stderr
            ]
        )
    Right (ExitSuccess, stdout, _) ->
      parseNodeVersionOutput stdout
        & left
          ( \e ->
              "Wasp failed to the parse `node` version provided by `node --version`.\n"
                <> (show e <> "\n")
                <> "This is most likely a bug in Wasp, please file an issue."
          )

-- | Extracts node version from the output of `node --version`.
parseNodeVersionOutput :: String -> Either P.ParseError SV.Version
parseNodeVersionOutput = P.parse nodeVersionParser ""
  where
    nodeVersionParser = skipAnyCharTillMatch SV.versionParser
    skipAnyCharTillMatch p = P.manyTill P.anyChar (P.lookAhead $ P.try p) >> p

nodeNotFoundMessage :: String
nodeNotFoundMessage = "`node` command not found!"

makeNodeUnknownErrorMessage :: IOError -> String
makeNodeUnknownErrorMessage err =
  unlines
    [ "An unknown error occured while trying to run `node --version`:",
      indent 2 $ show err
    ]
