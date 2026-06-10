module Wasp.Cli.Command.Doctor
  ( doctor,
  )
where

import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Error (catchIOError, tryIOError)
import qualified System.Info
import System.Process (readProcessWithExitCode)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Node.Version as NodeVersion
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (eitherToMaybe, trim)
import Wasp.Util.GitRev (gitRevDescription)
import qualified Wasp.Util.Network.Socket as Socket
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

type Check = IO CheckResult

type CheckResult = Either String String

-- | Runs a series of sanity checks on the user's environment and prints a
-- report. It is meant to help users (and us) figure out what might be wrong
-- with their setup.
--
-- It intentionally doesn't require a Wasp project and should never fail the
-- process: each check just reports its own result.
doctor :: IO ()
doctor = do
  putStrLn $ Term.applyStyles [Term.Bold] "Checking your environment...\n"

  forM_ checks $ \(title, check) -> do
    result <- check
    putStrLn $ renderCheckResult title result

renderCheckResult :: String -> CheckResult -> String
renderCheckResult title result =
  symbol ++ " " ++ title ++ ": " ++ detail
  where
    detail = either id id result
    symbol = case result of
      Right _ -> Term.applyStyles [Term.Green] "[✓]"
      Left _ -> Term.applyStyles [Term.Red] "[✗]"

-- | The checks to run, each as a (title, check) pair. A check returns the text
-- to print after the title: `Right` if it succeeded, `Left` if it failed.
checks :: [(String, Check)]
checks =
  [ ("Wasp", checkWasp),
    ("System", checkSystem),
    ("Node.js", checkNode),
    ("npm", checkNpm),
    ("Docker", checkDocker)
  ]
    ++ makePortChecks

checkWasp :: Check
checkWasp = return $ Right $ show waspVersion ++ gitInfo
  where
    gitInfo = maybe "" (\description -> " (git " ++ description ++ ")") gitRevDescription

checkSystem :: Check
checkSystem =
  Right . unwords . filter (not . null) . catMaybes
    <$> sequence
      [ return $ Just System.Info.os,
        eitherToMaybe <$> getOsVersion,
        return $ Just System.Info.arch
      ]
  where
    getOsVersion = tryIOError $ do
      (_exitCode, stdout, _stderr) <- readProcessWithExitCode "uname" ["-r"] ""
      return $ trim stdout

checkNode :: Check
checkNode = checkToolVersion NodeVersion.oldestWaspSupportedNodeVersion <$> NodeVersion.getUserNodeVersion

checkNpm :: Check
checkNpm = checkToolVersion NodeVersion.oldestWaspSupportedNpmVersion <$> NodeVersion.getUserNpmVersion

checkToolVersion :: SV.Version -> Either String SV.Version -> CheckResult
checkToolVersion _ (Left _) = Left "not found in PATH"
checkToolVersion oldestSupportedVersion (Right userVersion)
  | userVersion >= oldestSupportedVersion = Right (show userVersion)
  | otherwise =
      Left $
        show userVersion ++ " is too old, Wasp requires version " ++ show oldestSupportedVersion ++ " or higher"

checkDocker :: Check
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

makePortChecks :: [(String, Check)]
makePortChecks =
  makePortCheck
    <$> [ ("client", WebApp.defaultClientPort),
          ("server", Server.defaultServerPort),
          ("dev database", Dev.Postgres.defaultDevPort)
        ]
  where
    makePortCheck (usedFor, port) =
      ( "Port " ++ show port ++ " (" ++ usedFor ++ ")",
        do
          isInUse <- Socket.checkIfPortIsInUse $ Socket.makeLocalHostSocketAddress $ fromIntegral port
          return $ if isInUse then Left "in use" else Right "free"
      )
