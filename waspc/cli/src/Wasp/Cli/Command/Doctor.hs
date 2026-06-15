module Wasp.Cli.Command.Doctor
  ( doctor,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
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

type Check a = ExceptT String IO a

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
    result <- runExceptT check
    putStrLn $ renderCheckResult title result

-- | The checks to run, each as a (title, check) pair. A check returns the text
-- to print after the title: `Right` if it succeeded, `Left` if it failed.
checks :: [(String, Check String)]
checks =
  [ ("Wasp", checkWasp),
    ("System", checkSystem),
    ("Node.js", makeToolVersionCheck "node" NodeVersion.oldestWaspSupportedNodeVersion (ExceptT NodeVersion.getUserNodeVersion)),
    ("npm", makeToolVersionCheck "npm" NodeVersion.oldestWaspSupportedNpmVersion (ExceptT NodeVersion.getUserNpmVersion)),
    ("Docker", checkDocker >> return "running"),
    makePortCheck "Client" WebApp.defaultClientPort,
    makePortCheck "Server" Server.defaultServerPort,
    makePortCheck "Dev database" Dev.Postgres.defaultDevPort
  ]
  where
    makePortCheck name port =
      ( name ++ " port (" ++ show port ++ ")",
        checkPortIsFree port >> return "free"
      )

    makeToolVersionCheck name minVersion getCurrentVersion =
      checkToolExists name
        >> checkToolVersion minVersion getCurrentVersion
        <&> show

renderCheckResult :: String -> Either String String -> String
renderCheckResult title result =
  symbol ++ " " ++ title ++ ": " ++ detail
  where
    detail = either id id result
    symbol = case result of
      Right _ -> Term.applyStyles [Term.Green] "[✓]"
      Left _ -> Term.applyStyles [Term.Red] "[✗]"

checkWasp :: Check String
checkWasp = return $ show waspVersion ++ gitInfo
  where
    gitInfo = maybe "" (\description -> " (git " ++ description ++ ")") gitRevDescription

checkSystem :: Check String
checkSystem =
  unwords . filter (not . null) . catMaybes
    <$> sequence
      [ return $ Just System.Info.os,
        liftIO $ eitherToMaybe <$> getOsVersion,
        return $ Just System.Info.arch
      ]
  where
    getOsVersion = tryIOError $ do
      (_exitCode, stdout, _stderr) <- readProcessWithExitCode "uname" ["-r"] ""
      return $ trim stdout

checkDocker :: Check ()
checkDocker =
  checkToolExists "docker"
    >> liftIO isDockerDaemonRunning
    >>= \case
      False -> throwError "docker daemon is not running"
      True -> return ()
  where
    isDockerDaemonRunning =
      (isExitSuccess <$> readProcessWithExitCode "docker" ["info"] "")
        `catchIOError` const (return False)

    isExitSuccess (exitCode, _, _) = exitCode == ExitSuccess

checkToolVersion :: SV.Version -> Check SV.Version -> Check SV.Version
checkToolVersion minVersion getUserVersion = do
  userVersion <- getUserVersion
  when (userVersion < minVersion) $ do
    throwError $
      show userVersion ++ " (Wasp requires at least " ++ show minVersion ++ ")"
  return userVersion

checkToolExists :: String -> Check ()
checkToolExists toolName =
  liftIO (findExecutable toolName)
    >>= \case
      Just _ -> return ()
      Nothing -> throwError "not found in PATH"

checkPortIsFree :: Int -> Check ()
checkPortIsFree port =
  liftIO (checkIfLocalPortIsInuse port)
    >>= \case
      True -> throwError "in use"
      False -> return ()
  where
    checkIfLocalPortIsInuse = Socket.checkIfPortIsInUse . Socket.makeLocalHostSocketAddress . fromIntegral
