{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestCommands
  ( TestCommand (..),
    runTestCommand,
    shellCommand,
    appendToFile,
    replaceLineInFile,
    waspCliNewMinimalStarter,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import qualified System.IO as IO
import System.Process (callCommand)

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.
-- TODO: In future, find a good way to test `wasp-cli start`.

-- | Executes test actions with access to some context.
-- e.g. 'WaspProject.TestCommands.WaspProjectContext' assumes commands are run from inside a Wasp project.
-- It also provides access to context details like the command execution directory.
newtype TestCommand context a = TestCommand (ReaderT context IO a)
  deriving (Functor, Applicative, Monad, MonadReader context, MonadIO)

runTestCommand :: context -> TestCommand context a -> IO a
runTestCommand context (TestCommand reader) = runReaderT reader context

-- | Helper for executing shell commands within TestCommands.
-- This is used for commands like wasp-cli, git, docker that are cross-platform.
shellCommand :: String -> TestCommand context ()
shellCommand cmd = liftIO $ callCommand cmd

-- General commands

appendToFile :: FilePath -> String -> TestCommand context ()
appendToFile fileName content = liftIO $ IO.appendFile fileName (content ++ "\n")

-- | Replaces a specific line in a file (1-indexed).
-- Assumes the file has at least 'lineNumber' lines.
replaceLineInFile :: FilePath -> Int -> String -> TestCommand context ()
replaceLineInFile fileName lineNumber newLine = liftIO $ do
  contents <- readFile fileName
  let fileLines = lines contents
  -- Ensure file has enough lines
  if length fileLines < lineNumber
    then error $ "File " ++ fileName ++ " has only " ++ show (length fileLines) ++ " lines, cannot replace line " ++ show lineNumber
    else do
      let (before, _ : after) = splitAt (lineNumber - 1) fileLines
      let newContents = unlines (before ++ [newLine] ++ after)
      -- Force evaluation before writing to avoid lazy I/O issues
      length newContents `seq` writeFile fileName newContents

waspCliNewMinimalStarter :: String -> TestCommand context ()
waspCliNewMinimalStarter appName = shellCommand $ "wasp-cli new " ++ appName ++ " -t minimal"
