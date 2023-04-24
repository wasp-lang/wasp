module Wasp.Util.NodeCommand where

import System.Process (callCommand)
import UnliftIO.Exception (SomeException, try)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.NodeVersion as NodeVersion

runNodeCommand :: [String] -> IO (Either String ())
runNodeCommand command =
  NodeVersion.getNodeVersion >>= \case
    Left nodeVersionErrorMsg -> return $ Left nodeVersionErrorMsg
    Right nodeVersion ->
      if SV.isVersionInRange nodeVersion NodeVersion.nodeVersionRange
        then do
          try executeCommand >>= \case
            Left (e :: SomeException) -> return (Left $ show e)
            Right _ -> return $ Right ()
        else return $ Left (NodeVersion.makeNodeVersionMismatchMessage nodeVersion)
  where
    executeCommand = callCommand $ unwords command
