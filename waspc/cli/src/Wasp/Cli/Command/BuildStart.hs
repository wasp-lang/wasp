module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently_)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Function (on)
import Data.List (nubBy)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.Process (callProcess, createProcess, cwd, env, proc, waitForProcess)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))

clientPort :: Integer
clientPort = 3000

serverPort :: Integer
serverPort = 3001

waspAppName :: String
waspAppName = "wasp-app-example"

clientUrl :: String
clientUrl = "http://localhost:" ++ show clientPort

serverUrl :: String
serverUrl = "http://localhost:" ++ show serverPort

postgresUrl :: String
postgresUrl = "postgresql://postgres:devpass1234@localhost:5433/" ++ waspAppName ++ "-db"

buildStart :: Command ()
buildStart = do
  InWaspProject waspProjectDir <- require

  void build
  liftIO $ concurrently_ buildStartClient buildStartServer

buildStartClient :: IO ()
buildStartClient = do
  void $ callProcess_ "npm" ["run", "build"] ".wasp/build/web-app" [("REACT_APP_API_URL", serverUrl)]
  void $ callProcess_ "npx" ["vite", "preview", "--port", show clientPort] ".wasp/build/web-app" []

buildStartServer :: IO ()
buildStartServer = do
  jwtSecret <- randomSecret 32 <$> newStdGen

  void $ callProcess "docker" ["build", "-t", waspAppName, ".wasp/build"]
  void $
    callProcess
      "docker"
      ( ["run", "--rm"]
          ++ flags
            [ ("env-file", ".env.server"),
              ("network", "host")
            ]
          ++ envFlags
            [ ("DATABASE_URL", postgresUrl),
              ("WASP_WEB_CLIENT_URL", clientUrl),
              ("WASP_SERVER_URL", serverUrl),
              ("JWT_SECRET", jwtSecret)
            ]
          ++ [waspAppName]
      )
  where
    flags :: [(String, String)] -> [String]
    flags =
      concatMap (\(name, value) -> ["--" ++ name, value])

    envFlags :: [(String, String)] -> [String]
    envFlags =
      concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])

randomSecret :: RandomGen g => Int -> g -> String
randomSecret len gen =
  take len $ filter isAlphaNum $ randoms gen
  where
    isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

callProcess_ :: FilePath -> [String] -> FilePath -> [(String, String)] -> IO ExitCode
callProcess_ cmd args cwdArg envArg = do
  systemEnv <- getEnvironment
  let processEnv = deduplicate $ systemEnv ++ envArg
  (_, _, _, process) <- createProcess (proc cmd args) {cwd = Just cwdArg, env = Just processEnv}
  waitForProcess process
  where
    deduplicate :: Eq a => [(a, b)] -> [(a, b)]
    deduplicate =
      reverse
        >> nubBy ((==) `on` fst)
        >> reverse
