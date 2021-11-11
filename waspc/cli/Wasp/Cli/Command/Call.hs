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
  | Info
  | PrintBashCompletionInstruction
  | GenerateBashCompletionScript
  | BashCompletionListCommands
  | Unknown [String] -- all args
