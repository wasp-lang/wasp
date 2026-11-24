-- | Describing the commands that tests run.
module E2E.Command
  ( Command (..),
    Executable (..),
    StdinMode (..),
    command,
    waspCli,
    withEnv,
    withoutEnv,
    showCommand,
    defaultTimeoutSeconds,
    failFastTimeoutSeconds,
    cleanupCommandTimeoutSeconds,
  )
where

import qualified Data.Text as T
import Wasp.Project.Db (databaseUrlEnvVarName)

data Command = Command
  { executable :: Executable,
    arguments :: [String],
    -- | @Just value@ sets the variable, @Nothing@ removes it.
    environmentOverrides :: [(String, Maybe String)],
    stdinMode :: StdinMode,
    -- | How long 'E2E.Process.run' and its variants wait for the command to exit.
    timeoutSeconds :: Int
  }

data Executable
  = -- | The Wasp CLI under test. Shown as @wasp@ in logs.
    WaspCli
  | -- | A program on the PATH, or a path to one.
    Program String

data StdinMode
  = -- | The process reads the text and then EOF.
    StdinText T.Text
  | -- | Stdin stays open, without any data, until the process is stopped.
    OpenEmptyStdin

command :: String -> [String] -> Command
command program arguments =
  Command
    { executable = Program program,
      arguments,
      environmentOverrides = [],
      stdinMode = StdinText "",
      timeoutSeconds = defaultTimeoutSeconds
    }

-- | Long enough for the slowest commands we run, e.g. `npm install` of a big
-- project on a loaded CI runner, so a timeout only stops a command that hangs.
defaultTimeoutSeconds :: Int
defaultTimeoutSeconds = 15 * 60

-- | For commands that must fail before doing real work, e.g. before building
-- or serving an app. If the check regresses, we don't want to wait for the
-- default timeout.
failFastTimeoutSeconds :: Int
failFastTimeoutSeconds = 2 * 60

-- | For commands in cleanup steps, e.g. @docker rm --force@. Much shorter than
-- the default, since nothing can cut a hung cleanup short: cleanups run
-- masked, and we ignore Ctrl+C after the first one.
cleanupCommandTimeoutSeconds :: Int
cleanupCommandTimeoutSeconds = 60

-- | Runs without the test runner's @DATABASE_URL@. With it, Wasp would use
-- that database instead of the project's dev database, so e.g. a test's
-- `wasp db reset` would wipe it. Tests that need it set it with 'withEnv'.
waspCli :: [String] -> Command
waspCli arguments = withoutEnv [databaseUrlEnvVarName] $ (command "wasp" arguments) {executable = WaspCli}

withEnv :: [(String, String)] -> Command -> Command
withEnv variables cmd = cmd {environmentOverrides = cmd.environmentOverrides ++ [(key, Just value) | (key, value) <- variables]}

withoutEnv :: [String] -> Command -> Command
withoutEnv keys cmd = cmd {environmentOverrides = cmd.environmentOverrides ++ [(key, Nothing) | key <- keys]}

-- | The command as you would type it in a shell.
showCommand :: Command -> String
showCommand cmd = unwords $ envPrefix ++ map quoteIfNeeded (executableName : cmd.arguments)
  where
    executableName = case cmd.executable of
      WaspCli -> "wasp"
      Program program -> program
    envPrefix = unsetPrefix ++ [key ++ "=" ++ quoteIfNeeded value | (key, Just value) <- cmd.environmentOverrides]
    unsetPrefix = case [key | (key, Nothing) <- cmd.environmentOverrides] of
      [] -> []
      unsetKeys -> "env" : concatMap (\key -> ["-u", key]) unsetKeys
    quoteIfNeeded word
      | null word || any (`elem` (" \"'\n" :: String)) word = show word
      | otherwise = word
