-- | Describing the commands that tests run.
module E2E.Command
  ( Command (..),
    Executable (..),
    StdinMode (..),
    program,
    waspCli,
    nodeModulesBin,
    withEnv,
    withoutEnv,
    showCommand,
    defaultTimeoutSeconds,
    failFastTimeoutSeconds,
    cleanupCommandTimeoutSeconds,
  )
where

import Data.List (intercalate)
import qualified Data.Text as T
import qualified System.FilePath as FP

data Command = Command
  { executable :: Executable,
    arguments :: [String],
    -- | @Just value@ sets the variable, @Nothing@ removes it.
    environmentOverrides :: [(String, Maybe String)],
    -- | Directories to put first on @PATH@, relative to the working directory.
    extraPath :: [FilePath],
    stdinMode :: StdinMode,
    -- | How long 'E2E.Process.run' and its variants wait for the command to exit.
    timeoutSeconds :: Int
  }

data Executable
  = -- | The Wasp CLI under test. Shown as @wasp@ in logs.
    WaspCli
  | -- | Like in a shell, a path with a directory is relative to the working
    -- directory, and other names are looked up in @PATH@.
    Program String

data StdinMode
  = -- | The process reads the text and then EOF.
    StdinText T.Text
  | -- | Stdin stays open, without any data, until the process is stopped.
    OpenEmptyStdin

waspCli :: [String] -> Command
waspCli = command WaspCli

program :: String -> [String] -> Command
program = command . Program

command :: Executable -> [String] -> Command
command executable arguments =
  Command
    { executable = executable,
      arguments,
      environmentOverrides = [],
      extraPath = [],
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

-- | A binary of an npm package installed in the working directory. Like with
-- @npx@, the other binaries there are on its @PATH@. We don't use @npx@ itself
-- since it doesn't forward signals correctly in some environments.
nodeModulesBin :: String -> [String] -> Command
nodeModulesBin bin arguments = (program (binDir ++ "/" ++ bin) arguments) {extraPath = [binDir]}
  where
    binDir = "node_modules/.bin"

-- | Sets the variables, replacing what earlier 'withEnv' and 'withoutEnv'
-- calls did with them.
withEnv :: [(String, String)] -> Command -> Command
withEnv variables = overrideEnv [(key, Just value) | (key, value) <- variables]

-- | Removes the variables, replacing what earlier 'withEnv' and 'withoutEnv'
-- calls did with them.
withoutEnv :: [String] -> Command -> Command
withoutEnv keys = overrideEnv [(key, Nothing) | key <- keys]

overrideEnv :: [(String, Maybe String)] -> Command -> Command
overrideEnv overrides cmd =
  cmd {environmentOverrides = filter ((`notElem` map fst overrides) . fst) cmd.environmentOverrides ++ overrides}

-- | The command as you would type it in a shell.
showCommand :: Command -> String
showCommand cmd = unwords $ envPrefix ++ map quoteIfNeeded (executableName : cmd.arguments)
  where
    executableName = case cmd.executable of
      WaspCli -> "wasp"
      Program program' -> program'
    envPrefix = unsetPrefix ++ [key ++ "=" ++ quoteIfNeeded value | (key, Just value) <- cmd.environmentOverrides] ++ pathPrefix
    -- Last, since the extra path dirs go before the @PATH@ the overrides leave.
    pathPrefix = case cmd.extraPath of
      [] -> []
      dirs -> ["PATH=" ++ intercalate [FP.searchPathSeparator] (map quoteIfNeeded dirs ++ ["$PATH"])]
    unsetPrefix = case [key | (key, Nothing) <- cmd.environmentOverrides] of
      [] -> []
      unsetKeys -> "env" : concatMap (\key -> ["-u", key]) unsetKeys
    quoteIfNeeded word
      | null word || any (`elem` (" \"'\n" :: String)) word = show word
      | otherwise = word
