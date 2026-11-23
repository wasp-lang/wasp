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
  )
where

import qualified Data.Text as T

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
      timeoutSeconds = 15 * 60
    }

waspCli :: [String] -> Command
waspCli arguments = (command "wasp" arguments) {executable = WaspCli}

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
    envPrefix = [key ++ "=" ++ quoteIfNeeded value | (key, Just value) <- cmd.environmentOverrides]
    quoteIfNeeded word
      | null word || any (`elem` (" \"'\n" :: String)) word = show word
      | otherwise = word
