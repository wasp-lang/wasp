module Wasp.Cli.Command.Doctor
  ( doctor,
  )
where

import Control.Monad (forM_)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (catchIOError)
import qualified System.Info
import System.Process (readProcessWithExitCode)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Node.Version as NodeVersion
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (trim)
import Wasp.Util.GitRev (gitRevDescription)
import qualified Wasp.Util.Network.Socket as Socket
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

-- | Runs a series of sanity checks on the user's environment and prints a
-- report. It is meant to help users (and us) figure out what might be wrong with
-- their setup.
--
-- It is intentionally environment-level (it doesn't require a Wasp project) and
-- never fails the process: each check just reports its own result.
doctor :: IO ()
doctor = do
  putStrLn $ Term.applyStyles [Term.Bold] "Running Wasp doctor...\n"
  results <- mapM runCheck checks
  forM_ results $ putStrLn . uncurry renderCheckResult
  putStrLn ""
  putStrLn $ renderSummary results
  where
    runCheck (title, check) = (,) title <$> check

    renderCheckResult title result = symbol ++ " " ++ title ++ ": " ++ detail
      where
        (symbol, detail) = case result of
          Right successDetail -> (Term.applyStyles [Term.Green] "[✓]", successDetail)
          Left failureDetail -> (Term.applyStyles [Term.Red] "[✗]", failureDetail)

    renderSummary checkResults
      | numFailures > 0 = Term.applyStyles [Term.Red] $ show numFailures ++ " check(s) need attention."
      | otherwise = Term.applyStyles [Term.Green] "Everything looks good!"
      where
        numFailures = length [() | (_title, Left _) <- checkResults]

-- | The checks to run, each as a (title, check) pair. A check returns the text
-- to print after the title: `Right` if it succeeded, `Left` if it failed.
checks :: [(String, IO (Either String String))]
checks =
  [ ("Wasp", checkWasp),
    ("System", checkSystem),
    ("Node.js", checkNode),
    ("npm", checkNpm),
    ("Docker", checkDocker),
    (portTitle WebApp.defaultClientPort "web client", checkPort WebApp.defaultClientPort),
    (portTitle Server.defaultServerPort "server", checkPort Server.defaultServerPort),
    (portTitle Dev.Postgres.defaultDevPort "dev database", checkPort Dev.Postgres.defaultDevPort)
  ]
  where
    portTitle port usedFor = "Port " ++ show port ++ " (" ++ usedFor ++ ")"

checkWasp :: IO (Either String String)
checkWasp = return $ Right $ show waspVersion ++ gitInfo
  where
    gitInfo = maybe "" (\description -> " (git " ++ description ++ ")") gitRevDescription

checkSystem :: IO (Either String String)
checkSystem = do
  osVersion <- getOsVersion
  return $ Right $ unwords $ filter (not . null) [System.Info.os, osVersion, System.Info.arch]

checkNode :: IO (Either String String)
checkNode = checkTool NodeVersion.getUserNodeVersion NodeVersion.oldestWaspSupportedNodeVersion

checkNpm :: IO (Either String String)
checkNpm = checkTool NodeVersion.getUserNpmVersion NodeVersion.oldestWaspSupportedNpmVersion

checkTool :: IO (Either String SV.Version) -> SV.Version -> IO (Either String String)
checkTool getUserVersion oldestSupportedVersion =
  getUserVersion >>= \case
    Left _ -> return $ Left "not found in PATH"
    Right userVersion
      | userVersion >= oldestSupportedVersion -> return $ Right (show userVersion)
      | otherwise ->
          return $
            Left $
              show userVersion ++ " is too old, Wasp requires version " ++ show oldestSupportedVersion ++ " or higher"

checkDocker :: IO (Either String String)
checkDocker =
  findExecutable "docker" >>= \case
    Nothing -> return $ Left "not found in PATH"
    Just _ -> do
      isRunning <- isDockerDaemonRunning
      return $
        if isRunning
          then Right "installed and running"
          else Left "installed but not running"
  where
    isDockerDaemonRunning =
      (isExitSuccess <$> readProcessWithExitCode "docker" ["info"] "")
        `catchIOError` const (return False)
    isExitSuccess (exitCode, _stdout, _stderr) = exitCode == ExitSuccess

checkPort :: Int -> IO (Either String String)
checkPort port = do
  isInUse <- Socket.checkIfPortIsInUse $ Socket.makeLocalHostSocketAddress $ fromIntegral port
  return $ if isInUse then Left "in use" else Right "free"

-- | Returns the OS release/version via `uname -r`, or an empty string if it
-- can't be determined (e.g. on systems without `uname`).
getOsVersion :: IO String
getOsVersion = getVersionFromUname `catchIOError` const (return "")
  where
    getVersionFromUname = do
      (_exitCode, stdout, _stderr) <- readProcessWithExitCode "uname" ["-r"] ""
      return $ trim stdout
