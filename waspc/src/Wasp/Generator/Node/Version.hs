module Wasp.Generator.Node.Version
  ( getNodeVersion,
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

getNodeVersion :: IO (Either String SV.Version)
getNodeVersion = do
  (exitCode, stdout, stderr) <-
    P.readProcessWithExitCode "node" ["--version"] ""
      `catchIOError` ( \e ->
                         if isDoesNotExistError e
                           then return (ExitFailure 1, "", nodeNotFoundMessage)
                           else return (ExitFailure 1, "", nodeUnknownErrorMessage e)
                     )
  return $ case exitCode of
    ExitFailure _ ->
      Left
        ( unlines
            [ "Running 'node --version' failed: ",
              indent 2 stderr,
              waspNodeRequirementMessage
            ]
        )
    ExitSuccess -> case parseNodeVersion stdout of
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

nodeUnknownErrorMessage :: IOError -> String
nodeUnknownErrorMessage err =
  unlines
    [ "An unknown error occured while trying to run `node --version`:",
      indent 2 $ show err,
      "If `node` is installed and in PATH, this is most likely a bug in Wasp, please file an issue."
    ]

waspNodeRequirementMessage :: String
waspNodeRequirementMessage =
  unwords
    [ "Wasp requires node " ++ show nodeVersionRange ++ " to be installed and in PATH.",
      "Check Wasp docs for more details: https://wasp-lang.dev/docs/quick-start#requirements."
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
        [ "Your node version does not meet Wasp's requirements!",
          "You are running node " ++ show nodeVersion ++ "."
        ],
      waspNodeRequirementMessage
    ]
