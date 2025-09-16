{-# LANGUAGE TypeSynonymInstances #-}

module WaspApp.ShellCommands
  ( WaspAppContext (..),
    appendToWaspFile,
    appendToPrismaFile,
    setWaspDbToPSQL,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
    buildWaspDockerImage,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    appendToFile,
    combineShellCommands,
    replaceLineInFile,
    shellCommandsIf,
  )
import System.FilePath (joinPath, (</>))

-- | Context for commands which are run from inside of a Wasp app project.
-- Switching to this context means we are currently inside the Wasp app project directory.
data WaspAppContext = WaspAppContext
  {_waspAppName :: String}
  deriving (Show)

appendToWaspFile :: FilePath -> ShellCommandBuilder WaspAppContext ShellCommand
appendToWaspFile = appendToFile "main.wasp"

appendToPrismaFile :: FilePath -> ShellCommandBuilder WaspAppContext ShellCommand
appendToPrismaFile = appendToFile "schema.prisma"

-- NOTE: fragile, assumes line numbers do not change.
setWaspDbToPSQL :: ShellCommandBuilder WaspAppContext ShellCommand
-- Change DB to postgres by adding string at specific line so it still parses.
setWaspDbToPSQL = replaceLineInFile "schema.prisma" 2 "  provider = \"postgresql\""

buildWaspDockerImage :: ShellCommandBuilder WaspAppContext ShellCommand
buildWaspDockerImage = do
  waspAppContext <- ask
  let dockerImageTag = "waspc-e2e-tests-" ++ _waspAppName waspAppContext
   in return $
        shellCommandsIf
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
