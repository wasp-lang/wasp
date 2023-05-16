module Wasp.Cli.Command.Call where

data Call
  = New !Arguments
  | Start !StartArg
  | Clean
  | Uninstall
  | Compile
  | Db !DbArgs
  | Build
  | Version
  | Telemetry
  | Deps
  | Dockerfile
  | Info
  | PrintBashCompletionInstruction
  | GenerateBashCompletionScript
  | BashCompletionListCommands
  | WaspLS !WaspLSArgs
  | Deploy !Arguments -- deploy cmd passthrough args
  | Test !TestArgs -- "client" | "server", then test cmd passthrough args

data TestArgs = TestClient !Arguments | TestServer !Arguments

data StartArg = StartDb | StartNormal

data WaspLSArgs = WaspLSArgs
  { wslLogFile :: !(Maybe FilePath),
    wslUseStudio :: !Bool
  }

data DbMigrateDevArgs = DbMigrateDevArgs
  { dbmdaName :: !(Maybe String),
    dbmdaCreateOnly :: !Bool
  }

data DbArgs
  = DbStart
  | DbMigrateDev !DbMigrateDevArgs
  | DbReset
  | DbSeed !(Maybe String)
  | DbStudio

type Arguments = [String]
