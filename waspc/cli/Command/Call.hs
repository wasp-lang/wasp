module Command.Call where

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
  | PrintBashCompletionInstruction
  | GenerateBashCompletionScript
  | BashCompletionListCommands [String]
  | Unknown [String] -- all args
