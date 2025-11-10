{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandBuilder (..),
    buildShellCommand,
    (~|),
    (~&&),
    (~?),
    (~||),
    createFile,
    appendToFile,
    replaceLineInFile,
    waspCliNewMinimalStarter,
    waspCliVersion,
    waspCliTelemetry,
    waspCliCompletion,
  )
where

import Control.Monad.Reader (MonadReader, Reader, runReader)
import StrongPath (Path', Abs, fromAbsFile, parseRelFile, (</>), fromAbsDir, Dir)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base64 as B64

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.
-- TODO: In future, find a good way to test `wasp-cli start`.

type ShellCommand = String

-- | Builds shell command with access and assumptions to some context.
-- e.g. 'WaspProject.ShellCommands.WaspProjectContext' assumes commands are run from inside a Wasp project.
-- It also provides access to context details like the command execution directory.
newtype ShellCommandBuilder context a = ShellCommandBuilder (Reader context a)
  deriving (Functor, Applicative, Monad, MonadReader context)

buildShellCommand :: context -> ShellCommandBuilder context a -> a
buildShellCommand context (ShellCommandBuilder reader) = runReader reader context

-- Command utilities

-- | Pipe the output of one command into another.
(~|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~| cmd2 = cmd1 ++ " | " ++ cmd2

infixl 7 ~|

-- | Execute the second command only if the first command succeeds.
-- In case of failure, the command chain will stop.
(~&&) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~&& cmd2 = cmd1 ++ " && " ++ cmd2

infixl 6 ~&&

-- | Execute the second command only if the first command succeeds.
-- In case of failure, the command chain will stop.
(~||) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~|| cmd2 = cmd1 ++ " || " ++ cmd2

infixl 6 ~||

-- | Execute the second command only if the first command succeeds.
-- The command chain will continue regardless of whether the second command runs.
(~?) :: ShellCommand -> ShellCommand -> ShellCommand
(~?) condition command =
  "if " ++ condition ++ "; then " ++ command ++ " ;fi"

infixl 4 ~?

-- General commands

-- NOTE: Pretty fragile. Can't handle spaces in args, *nix only, etc.
-- TODO: Franjo: copied from old code, check if we can improve further.
createFile :: Path' Abs (Dir parentDir) -> String -> String -> ShellCommandBuilder context ShellCommand
createFile parentDir fileName fileContent = return $ createParentDirCmd ~&& writeContentsToFileCmd
  where
    createParentDirCmd = "mkdir -p " ++ fromAbsDir parentDir
    -- writeContentsToFileCmd = "cat << 'EOF' > " ++ fromAbsFile destinationFile ++ "\n" ++ fileContent ++ "\nEOF\n"
    writeContentsToFileCmd = "printf %s " ++ base64FileContent ++ " | base64 -d > " ++ fromAbsFile destinationFile

    base64FileContent = C8.unpack . B64.encode . C8.pack $ fileContent
    destinationFile = parentDir </> fromJust (parseRelFile fileName)

appendToFile :: FilePath -> String -> ShellCommandBuilder context ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (content ++ "\n") ++ " >> " ++ fileName

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder context ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    "awk 'NR=="
      ++ show lineNumber
      ++ "{$0="
      ++ show line
      ++ "}1' "
      ++ fileName
      ++ " > "
      ++ fileName
      ++ ".tmp"
        ~&& "mv "
      ++ fileName
      ++ ".tmp "
      ++ fileName

waspCliNewMinimalStarter :: String -> ShellCommandBuilder context ShellCommand
waspCliNewMinimalStarter appName = return $ "wasp-cli new " ++ appName ++ " -t minimal"

waspCliVersion :: ShellCommandBuilder context ShellCommand
waspCliVersion = return "wasp-cli version"

waspCliTelemetry :: ShellCommandBuilder context ShellCommand
waspCliTelemetry = return "wasp-cli telemetry"

waspCliCompletion :: ShellCommandBuilder context ShellCommand
waspCliCompletion = return "wasp-cli completion"
