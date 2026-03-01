module Wasp.Cli.Command.Call where

data Call
  = New Arguments
  | NewAi Arguments
  | Start
  | StartDb Arguments
  | Clean
  | Uninstall
  | Compile
  | Db Arguments -- db args
  | Build
  | BuildStart Arguments
  | Version
  | Telemetry
  | Deps
  | Dockerfile
  | Info
  | News
  | Studio
  | PrintBashCompletionInstruction
  | BashCompletionListCommands
  | WaspLS
  | Deploy Arguments -- deploy cmd passthrough args
  | Test Arguments -- "client" | "server", then test cmd passthrough args
  | ModuleCmd Arguments -- module subcommand args
  | Unknown Arguments -- all args

type Arguments = [String]
