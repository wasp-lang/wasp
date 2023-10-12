module Wasp.Node.Version
  ( getAndCheckNodeVersion,
    getNodeVersion,
    nodeVersionRange,
    latestMajorNodeVersion,
    waspNodeRequirementMessage,
    makeNodeVersionMismatchMessage,
  )
where

import Data.Conduit.Process.Typed (ExitCode (..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.Process as P
import Text.Read (readMaybe)
import qualified Text.Regex.TDFA as R
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (indent)

-- | Gets the installed node version, if any is installed, and checks that it
-- meets Wasp's version requirement.
--
-- Returns a string representing the error
-- condition if node's version could not be found or if the version does not
-- meet the requirements.
getAndCheckNodeVersion :: IO (Either String SV.Version)
getAndCheckNodeVersion =
  getNodeVersion >>= \case
    Left errorMsg -> return $ Left errorMsg
    Right nodeVersion ->
      if SV.isVersionInRange nodeVersion nodeVersionRange
        then return $ Right nodeVersion
        else return $ Left $ makeNodeVersionMismatchMessage nodeVersion

-- | Gets the installed node version, if any is installed, and returns it.
--
-- Returns a string representing the error condition if node's version could
-- not be found.
getNodeVersion :: IO (Either String SV.Version)
getNodeVersion = do
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
            [ procErr,
              waspNodeRequirementMessage
            ]
        )
    Right (ExitFailure code, _, stderr) ->
      Left
        ( unlines
            [ "Running `node --version` failed (exit code " ++ show code ++ "):",
              indent 2 stderr,
              waspNodeRequirementMessage
            ]
        )
    Right (ExitSuccess, stdout, _) -> case parseNodeVersion stdout of
      Nothing ->
        Left
          ( "Wasp failed to parse node version."
              ++ " This is most likely a bug in Wasp, please file an issue."
          )
      Just version -> Right version

parseNodeVersion :: String -> Maybe SV.Version
parseNodeVersion nodeVersionStr =
  case nodeVersionStr R.=~ ("v([^\\.]+).([^\\.]+).(.+)" :: String) of
    ((_, _, _, [majorStr, minorStr, patchStr]) :: (String, String, String, [String])) -> do
      mjr <- readMaybe majorStr
      mnr <- readMaybe minorStr
      ptc <- readMaybe patchStr
      return $ SV.Version mjr mnr ptc
    _ -> Nothing

nodeNotFoundMessage :: String
nodeNotFoundMessage = "`node` command not found!"

makeNodeUnknownErrorMessage :: IOError -> String
makeNodeUnknownErrorMessage err =
  unlines
    [ "An unknown error occured while trying to run `node --version`:",
      indent 2 $ show err
    ]

waspNodeRequirementMessage :: String
waspNodeRequirementMessage =
  unwords
    [ "Wasp requires Node " ++ show nodeVersionRange ++ " to be installed and in PATH.",
      "Check Wasp documentation for more details: https://wasp-lang.dev/docs/quick-start#requirements."
    ]

nodeVersionRange :: SV.Range
nodeVersionRange = SV.Range [SV.backwardsCompatibleWith latestNodeLTSVersion]

latestNodeLTSVersion :: SV.Version
latestNodeLTSVersion = SV.Version 18 12 0

-- | Latest concrete major node version supported by the nodeVersionRange, and
--   therefore by Wasp.
--   Here we assume that nodeVersionRange is using latestNodeLTSVersion as its basis.
--   TODO: instead of making assumptions, extract the latest major node version
--   directly from the nodeVersionRange.
latestMajorNodeVersion :: SV.Version
latestMajorNodeVersion = latestNodeLTSVersion

makeNodeVersionMismatchMessage :: SV.Version -> String
makeNodeVersionMismatchMessage nodeVersion =
  unlines
    [ unwords
        [ "Your Node version does not meet Wasp's requirements!",
          "You are running Node " ++ show nodeVersion ++ "."
        ],
      waspNodeRequirementMessage
    ]
