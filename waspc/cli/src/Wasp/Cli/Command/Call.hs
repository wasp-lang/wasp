module Wasp.Cli.Command.Call where

data Call
  = New String -- project name
  | Start
  | Clean
  | Compile
  | Db [String] -- db args
  | Build
  | Version
  | Telemetry
  | Deps
  | Dockerfile
  | Info
  | Update
  | PrintBashCompletionInstruction
  | GenerateBashCompletionScript
  | BashCompletionListCommands
  | WaspLS
  | Unknown [String] -- all args
