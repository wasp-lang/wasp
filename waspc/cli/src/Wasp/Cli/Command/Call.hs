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
  deriving (Show, Eq)

data TestArgs = TestClient !Arguments | TestServer !Arguments deriving (Show, Eq)

data StartArg = StartDb | StartNormal deriving (Show, Eq)

data WaspLSArgs = WaspLSArgs
  { wslLogFile :: !(Maybe FilePath),
    wslUseStudio :: !Bool
  }
  deriving (Show, Eq)

data DbMigrateDevArgs = DbMigrateDevArgs
  { dbmdaName :: !(Maybe String),
    dbmdaCreateOnly :: !Bool
  }
  deriving (Show, Eq)

data DbArgs
  = DbStart
  | DbMigrateDev !DbMigrateDevArgs
  | DbReset
  | DbSeed !(Maybe String)
  | DbStudio
  deriving (Show, Eq)

type Arguments = [String]
