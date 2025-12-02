{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandBuilder (..),
    WaspNewTemplate (..),
    buildShellCommand,
    (~|),
    (~&&),
    (~?),
    (~||),
    createFile,
    appendToFile,
    replaceLineInFile,
    waspCliVersion,
    waspCliNewInteractive,
    waspCliNew,
    waspCliTelemetry,
    waspCliCompletion,
  )
where

import Control.Monad.Reader (MonadReader, Reader, runReader)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import StrongPath (Abs, File', Path', fromAbsDir, fromAbsFile, parent)

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
(~&&) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 ~&& cmd2 = cmd1 ++ " && " ++ cmd2

infixl 6 ~&&

-- | Execute the second command only if the first command fails.
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

createFile :: Path' Abs File' -> T.Text -> ShellCommandBuilder context ShellCommand
createFile file fileContent = return $ createParentDir ~&& writeContentsToFile
  where
    createParentDir :: ShellCommand
    createParentDir = "mkdir -p " ++ fromAbsDir (parent file)

    writeContentsToFile :: ShellCommand
    writeContentsToFile = "printf %s " ++ base64FileContent ++ " | base64 -d > " ++ fromAbsFile file

    -- Using base64 encoding for file content helps us escape dealing with special characters.
    base64FileContent = C8.unpack . B64.encode . C8.pack . T.unpack $ fileContent

appendToFile :: FilePath -> T.Text -> ShellCommandBuilder context ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (T.unpack content ++ "\n") ++ " >> " ++ fileName

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder context ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    unwords
      ["awk", "'", ifLineNumberMatches, replaceWholeLine, printCurrentLine, "'", fileName, ">", tempFileName]
      ~&& unwords ["mv", tempFileName, fileName]
  where
    ifLineNumberMatches = "NR==" ++ show lineNumber
    replaceWholeLine = "{$0=" ++ show line ++ "}"
    printCurrentLine = "1"

    tempFileName = fileName ++ ".tmp"

data WaspNewTemplate = Minimal | Basic | SaaS

waspCliNewInteractive :: String -> WaspNewTemplate -> ShellCommandBuilder context ShellCommand
waspCliNewInteractive appName template =
  return $
    unwords
      [ "expect -c",
        "'",
        "spawn wasp-cli new;",
        "expect \"Enter the project name\";",
        "send -- \"" ++ appName ++ "\r\";",
        "expect \"Choose a starter template\";",
        "send -- \"" ++ templateNumber ++ "\r\";",
        "interact",
        "'"
      ]
  where
    templateNumber = case template of
      Basic -> "1"
      Minimal -> "2"
      SaaS -> "3"

waspCliNew :: String -> WaspNewTemplate -> ShellCommandBuilder context ShellCommand
waspCliNew appName template = return $ unwords ["wasp-cli", "new", appName, "-t", templateName]
  where
    templateName = case template of
      Basic -> "basic"
      Minimal -> "minimal"
      SaaS -> "saas"

waspCliVersion :: ShellCommandBuilder context ShellCommand
waspCliVersion = return "wasp-cli version"

waspCliTelemetry :: ShellCommandBuilder context ShellCommand
waspCliTelemetry = return "wasp-cli telemetry"

waspCliCompletion :: ShellCommandBuilder context ShellCommand
waspCliCompletion = return "wasp-cli completion"
