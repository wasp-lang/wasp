module Wasp.Cli.Command.Call where

data Call
  = New Arguments
  | Start
  | StartDb Arguments
  | Clean
  | Install
  | Uninstall
  | Compile
  | Db Arguments -- db args
  | Build
  | BuildStart Arguments
  | Version
  | Doctor
  | Telemetry
  | Deps
  | Dockerfile
  | Info
  | Inspect Arguments
  | News
  | Studio
  | PrintBashCompletionInstruction
  | BashCompletionListCommands
  | Deploy Arguments -- deploy cmd passthrough args
  | Test Arguments -- "client" | "server", then test cmd passthrough args
  | Unknown Arguments -- all args
  deriving (Eq)

type Arguments = [String]
