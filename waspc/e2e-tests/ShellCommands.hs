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
    createFile,
    insertCodeIntoFileAtLineNumber,
    replaceLineInFile,
    waspCliNewMinimalStarter,
  )
where

import Control.Monad.Reader (MonadReader, Reader, runReader)
import Data.List (intercalate)

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.
-- TODO: In future, find a good way to test `wasp-cli start`.

type ShellCommand = String

newtype ShellCommandBuilder ctx a = ShellCommandBuilder (Reader ctx a)
  deriving (Functor, Applicative, Monad, MonadReader ctx)

buildShellCommand :: ctx -> ShellCommandBuilder ctx a -> a
buildShellCommand ctx (ShellCommandBuilder reader) = runReader reader ctx

-- Command utilities

($|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 $| cmd2 = cmd1 ++ " | " ++ cmd2

combineShellCommands :: [ShellCommand] -> ShellCommand
combineShellCommands = intercalate " && "

shellCommandsIf :: ShellCommand -> [ShellCommand] -> ShellCommand
shellCommandsIf condition bodyCmds =
  "if " ++ condition ++ "; then " ++ combineShellCommands bodyCmds ++ " ;fi"

-- General commands

appendToFile :: FilePath -> String -> ShellCommandBuilder ctx ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (content ++ "\n") ++ " >> " ++ fileName

-- NOTE: Pretty fragile. Can't handle spaces in args, *nix only, etc.
createFile :: String -> FilePath -> String -> ShellCommandBuilder ctx ShellCommand
createFile content relDirFp filename = return $ combineShellCommands [createParentDir, writeContentsToFile]
  where
    createParentDir = "mkdir -p ./" ++ relDirFp
    destinationFile = "./" ++ relDirFp ++ "/" ++ filename
    contents = show (content ++ "\n")
    writeContentsToFile = unwords ["printf", contents, ">", destinationFile]

insertCodeIntoFileAtLineNumber :: FilePath -> Int -> String -> ShellCommandBuilder ctx ShellCommand
insertCodeIntoFileAtLineNumber fileName atLineNumber line =
  return $
    combineShellCommands
      [ "awk 'NR==" ++ show atLineNumber ++ "{print " ++ show line ++ "}1' " ++ fileName ++ " > " ++ fileName ++ ".tmp",
        "mv " ++ fileName ++ ".tmp " ++ fileName
      ]

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder ctx ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    combineShellCommands
      [ "awk 'NR==" ++ show lineNumber ++ "{$0=" ++ show line ++ "}1' " ++ fileName ++ " > " ++ fileName ++ ".tmp",
        "mv " ++ fileName ++ ".tmp " ++ fileName
      ]

waspCliNewMinimalStarter :: String -> ShellCommandBuilder ctx ShellCommand
waspCliNewMinimalStarter projectName = do
  return $
    "wasp-cli new " ++ projectName ++ " -t minimal"
