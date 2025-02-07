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
  | Version (Maybe String) Bool -- --force
  | Update Bool -- --force
  | Telemetry
  | Deps
  | Dockerfile
  | Info
  | Studio
  | PrintBashCompletionInstruction
  | GenerateBashCompletionScript
  | BashCompletionListCommands
  | WaspLS
  | Secret Arguments -- testing versioning passthrough args
  | Deploy Arguments -- deploy cmd passthrough args
  | Test Arguments -- "client" | "server", then test cmd passthrough args
  | Unknown Arguments -- all args

type Arguments = [String]
