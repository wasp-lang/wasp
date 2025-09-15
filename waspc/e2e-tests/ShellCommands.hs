{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ShellCommands
  ( ShellCommand,
    ShellCommandBuilder (..),
    GoldenTestContext (..),
    WaspAppContext (..),
    buildShellCommand,
    ($|),
    combineShellCommands,
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
    copyContentsOfGitTrackedDirToGoldenTestProject,
    withInSnapshotProjectDir,
  )
where

import Common
  ( GitRepositoryRoot,
    gitRootFromSnapshotProjectDir,
    snapshotProjectDirInSnapshotDir,
  )
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

newtype ShellCommandBuilder ctx a = ShellCommandBuilder (Reader ctx a)
  deriving (Functor, Applicative, Monad, MonadReader ctx)

-- | Context for commands which are run strictly from the golden tests.
data GoldenTestContext = GoldenTestContext
  {_goldenTestProjectName :: String}
  deriving (Show)

-- | Context for commands which are run from inside of a Wasp app project.
-- Switching to this context means we are currently inside the Wasp app project directory.
data WaspAppContext = WaspAppContext
  {_waspAppName :: String}
  deriving (Show)

buildShellCommand :: ctx -> ShellCommandBuilder ctx a -> a
buildShellCommand ctx (ShellCommandBuilder reader) = runReader reader ctx

withInSnapshotProjectDir :: ShellCommandBuilder WaspAppContext [ShellCommand] -> ShellCommandBuilder GoldenTestContext ShellCommand
withInSnapshotProjectDir commandBuilder = do
  goldenTestContext <- ask
  let waspAppName = _goldenTestProjectName goldenTestContext

  let snapshotProjectDir = snapshotProjectDirInSnapshotDir waspAppName
  let cdCommand :: ShellCommand = "cd " ++ fromRelDir snapshotProjectDir

  let waspAppContext = WaspAppContext waspAppName
  let commands = combineShellCommands (cdCommand : buildShellCommand waspAppContext commandBuilder)

  return commands

-- Command utilities

($|) :: ShellCommand -> ShellCommand -> ShellCommand
cmd1 $| cmd2 = cmd1 ++ " | " ++ cmd2

combineShellCommands :: [ShellCommand] -> ShellCommand
combineShellCommands = intercalate " && "

executeShellCommandsIf :: ShellCommand -> [ShellCommand] -> ShellCommand
executeShellCommandsIf condition bodyCmds =
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

copyContentsOfGitTrackedDirToGoldenTestProject ::
  Path' (Rel GitRepositoryRoot) (Dir src) ->
  ShellCommandBuilder GoldenTestContext ShellCommand
copyContentsOfGitTrackedDirToGoldenTestProject srcDirInGitRoot = do
  goldenTestContext <- ask
  let srcDirPath = fromRelDir (gitRootFromSnapshotProjectDir SP.</> srcDirInGitRoot)
      destDirPath = "./" ++ _goldenTestProjectName goldenTestContext

      createDestDir :: ShellCommand = "mkdir -p " ++ destDirPath

      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootFromSnapshotProjectDir ++ " ls-files " ++ fromRelDir srcDirInGitRoot
      -- Remove the src dir prefix from each path so that files get copied into the destination dir directly.
      -- e.g. `waspc/examples/todoApp/file.txt` -> `file.txt`
      stripSrcDirPrefixFromPaths :: ShellCommand =
        "sed 's#^" ++ fromRelDir srcDirInGitRoot ++ "##'"
      copyFromSrcDirToDestDir :: ShellCommand =
        "rsync -a --files-from=- " ++ srcDirPath ++ " " ++ destDirPath
   in return $
        combineShellCommands
          [ createDestDir,
            listRelPathsOfGitTrackedFilesInSrcDir
              $| stripSrcDirPrefixFromPaths
              $| copyFromSrcDirToDestDir
          ]

waspCliNewMinimalStarter :: String -> ShellCommandBuilder ctx ShellCommand
waspCliNewMinimalStarter projectName = do
  return $
    "wasp-cli new " ++ projectName ++ " -t minimal"

-- Wasp app commands

appendToWaspFile :: FilePath -> ShellCommandBuilder WaspAppContext ShellCommand
appendToWaspFile = appendToFile "main.wasp"

appendToPrismaFile :: FilePath -> ShellCommandBuilder WaspAppContext ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

-- NOTE: very fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder WaspAppContext ShellCommand
-- Change DB to postgres by adding string at specific line so it still parses.
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

buildDockerImage :: ShellCommandBuilder WaspAppContext ShellCommand
buildDockerImage = do
  waspAppContext <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _waspAppName waspAppContext
   in return $
        executeShellCommandsIf
          "[ -z \"$WASP_E2E_TESTS_SKIP_DOCKER\" ]"
          [ "cd .wasp/build",
            "docker build --build-arg \"BUILDKIT_DOCKERFILE_CHECK=error=true\" -t " ++ dockerImageTag ++ " .",
            "docker image rm " ++ dockerImageTag,
            "cd ../.."
          ]

waspCliCompile :: ShellCommandBuilder WaspAppContext ShellCommand
waspCliCompile = return "wasp-cli compile"

-- TODO: We need to be careful what migration names we accept here, as Prisma will
--       normalize a migration name containing spaces/dashes to underscores (and maybe other rules).
--       This will impact our ability to move directories around if the names do not match exactly.
--       Put in some check eventually.
waspCliMigrate :: String -> ShellCommandBuilder WaspAppContext ShellCommand
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

waspCliBuild :: ShellCommandBuilder WaspAppContext ShellCommand
waspCliBuild = return "wasp-cli build"
