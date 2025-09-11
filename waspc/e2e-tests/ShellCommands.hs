{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandContext (..),
    ShellCommandBuilder (..),
    runShellCommandBuilder,
    ($|),
    combineShellCommands,
    cdIntoCurrentProject,
    appendToWaspFile,
    appendToPrismaFile,
    createFile,
    setWaspDbToPSQL,
    waspCliNewMinimalStarter,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    buildDockerImage,
    insertCodeIntoFileAtLineNumber,
    copyContentsOfGitTrackedDirToCurrentProject,
  )
where

import Common (GitRepositoryRoot, gitRootFromGoldenTestProjectDir)
import Control.Monad.Reader (MonadReader (ask), Reader, runReader)
import Data.List (intercalate)
import StrongPath (Dir, Path', Rel, fromRelDir)
import qualified StrongPath as SP
import System.FilePath (joinPath, (</>))

-- NOTE: Should we consider separating shell parts, Wasp CLI parts, and test helper parts in the future?
--       This would likely reqiure adoption of some library, and creating new layers of abstraction.
--       Deemed not worth it right now by all.

-- NOTE: Using `wasp-cli` herein so we can assume using latest `cabal install` in CI and locally.

-- TODO: In future, find a good way to test `wasp-cli start`.

type ShellCommand = String

-- Each shell command gets access to the current project name, and maybe other things in future.
data ShellCommandContext = ShellCommandContext
  {_projectName :: String}
  deriving (Show)

-- Used to construct shell commands, while still giving access to the context (if needed).
newtype ShellCommandBuilder a = ShellCommandBuilder
  {_runShellCommandBuilder :: Reader ShellCommandContext a}
  deriving (Functor, Applicative, Monad, MonadReader ShellCommandContext)

runShellCommandBuilder :: ShellCommandBuilder a -> ShellCommandContext -> a
runShellCommandBuilder shellCommandBuilder context =
  runReader (_runShellCommandBuilder shellCommandBuilder) context

-- Commands Utilities

($|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 $| cmd2 = intercalate " | " [cmd1, cmd2]

combineShellCommands :: [ShellCommand] -> ShellCommand
combineShellCommands = intercalate " && "

conditionShellCommands :: ShellCommand -> [ShellCommand] -> ShellCommand
conditionShellCommands condition cmds =
  "if " ++ condition ++ "; then " ++ combineShellCommands cmds ++ " ;fi"

-- General commands

appendToFile :: FilePath -> String -> ShellCommandBuilder ShellCommand
appendToFile fileName content =
  -- NOTE: Using `show` to preserve newlines in string.
  return $ "printf " ++ show (content ++ "\n") ++ " >> " ++ fileName

-- NOTE: Pretty fragile. Can't handle spaces in args, *nix only, etc.
createFile :: String -> FilePath -> String -> ShellCommandBuilder ShellCommand
createFile content relDirFp filename = return $ combineShellCommands [createParentDir, writeContentsToFile]
  where
    createParentDir = "mkdir -p ./" ++ relDirFp
    destinationFile = "./" ++ relDirFp ++ "/" ++ filename
    contents = show (content ++ "\n")
    writeContentsToFile = unwords ["printf", contents, ">", destinationFile]

insertCodeIntoFileAtLineNumber :: FilePath -> Int -> String -> ShellCommandBuilder ShellCommand
insertCodeIntoFileAtLineNumber fileName atLineNumber line =
  return $
    combineShellCommands
      [ "awk 'NR==" ++ show atLineNumber ++ "{print " ++ show line ++ "}1' " ++ fileName ++ " > " ++ fileName ++ ".tmp",
        "mv " ++ fileName ++ ".tmp " ++ fileName
      ]

replaceLineInFile :: FilePath -> Int -> String -> ShellCommandBuilder ShellCommand
replaceLineInFile fileName lineNumber line =
  return $
    combineShellCommands
      [ "awk 'NR==" ++ show lineNumber ++ "{$0=" ++ show line ++ "}1' " ++ fileName ++ " > " ++ fileName ++ ".tmp",
        "mv " ++ fileName ++ ".tmp " ++ fileName
      ]

cdIntoCurrentProject :: ShellCommandBuilder ShellCommand
cdIntoCurrentProject = do
  context <- ask
  return $ "cd " ++ _projectName context

copyContentsOfGitTrackedDirToCurrentProject :: Path' (Rel GitRepositoryRoot) (Dir src) -> ShellCommandBuilder ShellCommand
copyContentsOfGitTrackedDirToCurrentProject srcDirInGitRoot = do
  context <- ask
  let srcDirPath = fromRelDir (gitRootFromGoldenTestProjectDir SP.</> srcDirInGitRoot)
      destinationDirPath = "./" ++ _projectName context

      createDestinationDir :: ShellCommand = "mkdir -p " ++ destinationDirPath

      listSrcDirGitTrackedFiles :: ShellCommand =
        "git -C " ++ fromRelDir gitRootFromGoldenTestProjectDir ++ " ls-files " ++ fromRelDir srcDirInGitRoot
      -- Remove the src dir prefix from each path so that files get copied into the destination dir directly.
      -- e.g. `waspc/examples/todoApp/file.txt` -> `file.txt`
      removeSrcDirPrefixFromPath :: ShellCommand =
        "sed 's#^" ++ fromRelDir srcDirInGitRoot ++ "##'"
      copyFilesFromSrcToDestination :: ShellCommand =
        "rsync -a --files-from=- " ++ srcDirPath ++ " " ++ destinationDirPath
   in return $
        combineShellCommands
          [ createDestinationDir,
            listSrcDirGitTrackedFiles $| removeSrcDirPrefixFromPath $| copyFilesFromSrcToDestination
          ]

-- Wasp project commands

appendToWaspFile :: FilePath -> ShellCommandBuilder ShellCommand
appendToWaspFile = appendToFile "main.wasp"

appendToPrismaFile :: FilePath -> ShellCommandBuilder ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

-- NOTE: very fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder ShellCommand
-- Change DB to postgres by adding string at specific line so it still parses.
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

buildDockerImage :: ShellCommandBuilder ShellCommand
buildDockerImage = do
  context <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _projectName context
   in return $
        conditionShellCommands
          "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          [ "cd .wasp/build",
            "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t " ++ dockerImageTag ++ " .",
            "docker image rm " ++ dockerImageTag,
            "cd ../.."
          ]

-- Wasp CLI commands

waspCliNewMinimalStarter :: ShellCommandBuilder ShellCommand
waspCliNewMinimalStarter = do
  context <- ask
  return $
    "wasp-cli new " ++ _projectName context ++ " -t minimal"

waspCliCompile :: ShellCommandBuilder ShellCommand
waspCliCompile = return "wasp-cli compile"

-- TODO: We need to be careful what migration names we accept here, as Prisma will
--       normalize a migration name containing spaces/dashes to underscores (and maybe other rules).
--       This will impact our ability to move directories around if the names do not match exactly.
--       Put in some check eventually.
waspCliMigrate :: String -> ShellCommandBuilder ShellCommand
waspCliMigrate migrationName =
  let generatedMigrationsDir = joinPath [".wasp", "out", "db", "migrations"]
      waspMigrationsDir = "migrations"
   in return $
        combineShellCommands
          [ -- Migrate using a migration name to avoid Prisma asking via CLI.
            "wasp-cli db migrate-dev --name " ++ migrationName,
            -- Rename both migrations to remove the date-specific portion of the directory to something static.
            "mv " ++ (waspMigrationsDir </> ("*" ++ migrationName)) ++ " " ++ (waspMigrationsDir </> ("no-date-" ++ migrationName)),
            "mv " ++ (generatedMigrationsDir </> ("*" ++ migrationName)) ++ " " ++ (generatedMigrationsDir </> ("no-date-" ++ migrationName))
          ]

waspCliBuild :: ShellCommandBuilder ShellCommand
waspCliBuild = return "wasp-cli build"
