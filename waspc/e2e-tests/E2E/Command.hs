-- | Describing the commands that tests run.
module E2E.Command
  ( Command (..),
    StdinMode (..),
    command,
    waspCli,
    nodeModulesBin,
    withEnv,
    withoutEnv,
    showCommand,
  )
where

import qualified Data.Text as T

data Command = Command
  { -- | Like in a shell, a path with a directory is relative to the working
    -- directory, and other names are looked up in @PATH@. Except for @wasp@,
    -- which is the Wasp CLI under test.
    executable :: String,
    arguments :: [String],
    -- | @Just value@ sets the variable, @Nothing@ removes it.
    environmentOverrides :: [(String, Maybe String)],
    -- | Directories to put first on @PATH@, relative to the working directory.
    extraPath :: [FilePath],
    stdinMode :: StdinMode,
    -- | How long 'E2E.Process.run' and its variants wait for the command to exit.
    timeoutSeconds :: Int
  }

data StdinMode
  = -- | The process reads the text and then EOF.
    StdinText T.Text
  | -- | Stdin stays open, without any data, until the process is stopped.
    OpenEmptyStdin

command :: String -> [String] -> Command
command executable arguments =
  Command
    { executable,
      arguments,
      environmentOverrides = [],
      extraPath = [],
      stdinMode = StdinText "",
      timeoutSeconds = 15 * 60
    }

waspCli :: [String] -> Command
waspCli = command "wasp"

-- | A binary of an npm package installed in the working directory. Like with
-- @npx@, the other binaries there are on its @PATH@. We don't use @npx@ itself
-- since it doesn't forward signals correctly in some environments.
nodeModulesBin :: String -> [String] -> Command
nodeModulesBin bin arguments = (command (binDir ++ "/" ++ bin) arguments) {extraPath = [binDir]}
  where
    binDir = "node_modules/.bin"

withEnv :: [(String, String)] -> Command -> Command
withEnv variables cmd = cmd {environmentOverrides = cmd.environmentOverrides ++ [(key, Just value) | (key, value) <- variables]}

withoutEnv :: [String] -> Command -> Command
withoutEnv keys cmd = cmd {environmentOverrides = cmd.environmentOverrides ++ [(key, Nothing) | key <- keys]}

-- | The command as you would type it in a shell.
showCommand :: Command -> String
showCommand cmd = unwords $ envPrefix ++ map quoteIfNeeded (cmd.executable : cmd.arguments)
  where
    envPrefix = [key ++ "=" ++ quoteIfNeeded value | (key, Just value) <- cmd.environmentOverrides]
    quoteIfNeeded word
      | null word || any (`elem` (" \"'\n" :: String)) word = show word
      | otherwise = word
