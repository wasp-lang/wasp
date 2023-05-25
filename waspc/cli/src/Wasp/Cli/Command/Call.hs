module Wasp.Cli.Command.Call where

data Call
  = New !NewArgs
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
  | Completion !CompletionArgs
  | WaspLS !WaspLSArgs
  | Deploy !Arguments -- deploy cmd passthrough args
  | Test !TestArgs -- "client" | "server", then test cmd passthrough args
  deriving (Show, Eq)

data Shell = Bash | Zsh | Fish deriving (Show, Eq)

data CompletionArgs = ShowInstruction | Generate !Shell deriving (Show, Eq)

data NewArgs = NewArgs
  { newProjectName :: !(Maybe String),
    newTemplateName :: !(Maybe String)
  }
  deriving (Show, Eq)

data TestArgs = TestClient !Arguments | TestServer !Arguments deriving (Show, Eq)

data StartArg = StartDb | StartApp deriving (Show, Eq)

data WaspLSArgs = WaspLSArgs
  { wlsLogFile :: !(Maybe FilePath),
    wlsUseStudio :: !Bool
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
