module Wasp.Cli.Command.Call where

data Call
  = New !Arguments
  | Start !StartArg
  | Clean
  | Uninstall
  | Compile
  | Db !Arguments -- db args
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
    waslUseStdio :: !Bool
  }

type Arguments = [String]
