module Wasp.Cli.Command.Call where

data Call
  = New Arguments
  | NewAi Arguments
  | Start
  | StartDb
  | Clean
  | Uninstall
  | TsSetup
  | Compile
  | Db Arguments -- db args
  | Build
  | BuildStart Arguments
  | Version
  | Telemetry
  | Deps
  | Dockerfile
  | Info
  | Studio
  | PrintBashCompletionInstruction
  | BashCompletionListCommands
  | WaspLS
  | Deploy Arguments -- deploy cmd passthrough args
  | Test Arguments -- "client" | "server", then test cmd passthrough args
  | Unknown Arguments -- all args

type Arguments = [String]
