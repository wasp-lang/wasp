{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandBuilder (..),
    buildShellCommand,
    ($|),
    combineShellCommands,
    shellCommandsIf,
    appendToFile,
    replaceLineInFile,
  )
where

import Control.Monad.Reader (MonadReader, Reader, runReader)
import Data.List (intercalate)

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.
-- TODO: In future, find a good way to test `wasp-cli start`.

type ShellCommand = String

-- | Builds shell command with access and assumptions to some context.
-- e.g. WaspAppContext assumes commands are run from inside a Wasp app project.
-- It also provides access to context details like Wasp app name, etc.
newtype ShellCommandBuilder context a = ShellCommandBuilder (Reader context a)
  deriving (Functor, Applicative, Monad, MonadReader context)

buildShellCommand :: context -> ShellCommandBuilder context a -> a
buildShellCommand context (ShellCommandBuilder reader) = runReader reader context

-- Command utilities

($|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 $| cmd2 = cmd1 ++ " | " ++ cmd2

combineShellCommands :: [ShellCommand] -> ShellCommand
combineShellCommands = intercalate " && "

shellCommandsIf :: ShellCommand -> [ShellCommand] -> ShellCommand
shellCommandsIf condition bodyCmds =
  "if " ++ condition ++ "; then " ++ combineShellCommands bodyCmds ++ " ;fi"

-- General commands

appendToFile :: FilePath -> String -> ShellCommandBuilder context ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (content ++ "\n") ++ " >> " ++ fileName

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder context ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    combineShellCommands
      [ "awk 'NR==" ++ show lineNumber ++ "{$0=" ++ show line ++ "}1' " ++ fileName ++ " > " ++ fileName ++ ".tmp",
        "mv " ++ fileName ++ ".tmp " ++ fileName
      ]
