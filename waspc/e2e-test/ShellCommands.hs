module ShellCommands
  ( ShellCommand,
    ShellCommandContext (..),
    MakeShellCommand,
    combineMakeShellCommands,
    combineShellCommands,
    cdIntoCurrentProject,
    appendToWaspFile,
    setDbToPSQL,
    waspCliNew,
    waspCliCompile,
    waspCliMigrate,
    waspCliBuild,
  )
where

import Data.List (intercalate)
import System.FilePath (joinPath, (</>))

-- TODO: Should we consider separating shell parts, Wasp CLI parts, and test helper parts in the future?
--       This would likely reqiure adoption of some library, and creating new layers of abstraction.
--       Deemed not worth it right now by all.

-- TODO: In future, find a good way to test `wasp-cli start`.

-- NOTE: Using `wasp-cli` herein so we can assume using latest `stack install` in CI and locally.

type ShellCommand = String

-- A way to create a shell command given some context info.
type MakeShellCommand = ShellCommandContext -> ShellCommand

-- Each shell command gets access to the current project name and current/golden output directories.
data ShellCommandContext = ShellCommandContext
  { _ctxtCurrentProjectName :: String,
    _ctxtCurrentOutputDirAbsFp :: FilePath,
    _ctxtGoldenOutputDirAbsFp :: FilePath
  }
  deriving (Show)

combineShellCommands :: [ShellCommand] -> ShellCommand
combineShellCommands = intercalate " && "

combineMakeShellCommands :: [MakeShellCommand] -> MakeShellCommand
combineMakeShellCommands commands context = combineShellCommands (map ($ context) commands)

cdIntoCurrentProject :: MakeShellCommand
cdIntoCurrentProject context = "cd " ++ _ctxtCurrentProjectName context

appendToWaspFile :: String -> MakeShellCommand
appendToWaspFile content _context =
  -- NOTE: Using `show` to preserve newlines in string.
  "echo " ++ show content ++ " >> main.wasp"

-- NOTE: This is fragile and will likely break in future. Assumes `app` decl is first line and by default
--       we do not have a `db` field. Consider better alternatives.
setDbToPSQL :: MakeShellCommand
setDbToPSQL _context =
  combineShellCommands
    [ -- Change DB to postgres by adding string at specific line so it still parses.
      "awk 'NR==2{print \"  db: { system: PostgreSQL },\"}1' main.wasp > main.wasp.tmp",
      "mv main.wasp.tmp main.wasp"
    ]

waspCliNew :: MakeShellCommand
waspCliNew context = "wasp-cli new " ++ _ctxtCurrentProjectName context

waspCliCompile :: MakeShellCommand
waspCliCompile _context = "wasp-cli compile"

-- TODO: We need to be careful what migration names we accept here, as Prisma will
--       normalize a migration name containing spaces/dashes to underscores (and maybe other rules).
--       This will impact our ability to move directories around if the names do not match exactly.
--       Put in some check eventually.
waspCliMigrate :: String -> MakeShellCommand
waspCliMigrate migrationName _context =
  let generatedMigrationsDir = joinPath [".wasp", "out", "db", "migrations"]
      waspMigrationsDir = "migrations"
   in combineShellCommands
        [ -- Migrate using a migration name to avoid Prisma asking via CLI.
          "wasp-cli db migrate-dev " ++ migrationName,
          -- Rename both migrations to remove the date-specific portion of the directory to something static.
          "mv " ++ (waspMigrationsDir </> ("*" ++ migrationName)) ++ " " ++ (waspMigrationsDir </> ("no-date-" ++ migrationName)),
          "mv " ++ (generatedMigrationsDir </> ("*" ++ migrationName)) ++ " " ++ (generatedMigrationsDir </> ("no-date-" ++ migrationName))
        ]

waspCliBuild :: MakeShellCommand
waspCliBuild _context = "wasp-cli build"
