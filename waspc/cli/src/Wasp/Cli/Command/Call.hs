module Wasp.Cli.Command.Call where

data Call
  = New String -- project name
  | NewAIHuman
  | NewAIMachine String String -- appName and appDescription
  | Start
  | StartDb
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
  | Unknown [String] -- all args
