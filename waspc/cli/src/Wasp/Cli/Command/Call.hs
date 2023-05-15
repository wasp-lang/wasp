module Wasp.Cli.Command.Call where

data Call
  = New !Arguments
  | Start 
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
  | WaspLS
  | Deploy !Arguments -- deploy cmd passthrough args
  | Test !Arguments -- "client" | "server", then test cmd passthrough args

type Arguments = [String]
