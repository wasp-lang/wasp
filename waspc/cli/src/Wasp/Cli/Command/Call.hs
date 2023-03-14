module Wasp.Cli.Command.Call where

data Call
  = New String -- project name
  | Start
  | Clean
  | Uninstall
  | Compile
  | Db [String] -- db args
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
  | Deploy [String] -- deploy cmd passthrough args
  | Test
  | Unknown [String] -- all args
