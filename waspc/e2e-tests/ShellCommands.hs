{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandBuilder (..),
    buildShellCommand,
    ($|),
    ($&&),
    ($?),
    appendToFile,
    replaceLineInFile,
  )
where

import Control.Monad.Reader (MonadReader, Reader, runReader)

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.
-- TODO: In future, find a good way to test `wasp-cli start`.

type ShellCommand = String

-- | Builds shell command with access and assumptions to some context.
-- e.g. 'WaspApp.ShellCommands.WaspAppContext' assumes commands are run from inside a Wasp app project.
-- It also provides access to context details like the Wasp app's name.
newtype ShellCommandBuilder context a = ShellCommandBuilder (Reader context a)
  deriving (Functor, Applicative, Monad, MonadReader context)

buildShellCommand :: context -> ShellCommandBuilder context a -> a
buildShellCommand context (ShellCommandBuilder reader) = runReader reader context

-- Command utilities

-- | Pipe the output of one command into another.
($|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 $| cmd2 = cmd1 ++ " | " ++ cmd2

-- | Execute the second command only if the first command succeeds.
-- The command chain will continue regardless of whether the second command runs.
($?) :: ShellCommand -> ShellCommand -> ShellCommand
($?) condition command =
  "if " ++ condition ++ "; then " ++ command ++ " ;fi"

-- | Execute the second command only if the first command succeeds.
-- In case of failure, the command chain will stop.
($&&) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 $&& cmd2 = cmd1 ++ " && " ++ cmd2

-- General commands

appendToFile :: FilePath -> String -> ShellCommandBuilder context ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (content ++ "\n") ++ " >> " ++ fileName

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder context ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    "awk 'NR==" ++ show lineNumber ++ "{$0=" ++ show line ++ "}1' " ++ fileName ++ " > " ++ fileName ++ ".tmp"
      $&& "mv " ++ fileName ++ ".tmp " ++ fileName
