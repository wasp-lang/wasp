module Wasp.Cli.Command.Call where

data Call
  = New Arguments
  | Start Arguments
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
  | Help
  | Telemetry
  | Deps
  | Dockerfile
  | Show Arguments
  | News
  | Studio
  | PrintBashCompletionInstruction
  | BashCompletionListCommands
  | Deploy Arguments -- deploy cmd passthrough args
  | Test Arguments -- "client" | "server", then test cmd passthrough args
  | Unknown Arguments -- all args
  deriving (Eq, Show)

type Arguments = [String]

-- | Parses CLI arguments into the command to run.
parseCall :: [String] -> Call
parseCall args = case args of
  ("new" : newArgs) -> New newArgs
  ("start" : "db" : startDbArgs) -> StartDb startDbArgs
  ("start" : startArgs) -> Start startArgs
  ["clean"] -> Clean
  ["install"] -> Install
  ["compile"] -> Compile
  ("db" : dbArgs) -> Db dbArgs
  ["uninstall"] -> Uninstall
  ["version"] -> Version
  ["doctor"] -> Doctor
  ["build"] -> Build
  ("build" : "start" : buildStartArgs) -> BuildStart buildStartArgs
  ["help"] -> Help
  ["--help"] -> Help
  ["-h"] -> Help
  ["telemetry"] -> Telemetry
  ["deps"] -> Deps
  ["dockerfile"] -> Dockerfile
  ("show" : showArgs) -> Show showArgs
  ["news"] -> News
  ["studio"] -> Studio
  ["completion"] -> PrintBashCompletionInstruction
  ["completion:list"] -> BashCompletionListCommands
  ("deploy" : deployArgs) -> Deploy deployArgs
  ("test" : testArgs) -> Test testArgs
  [] -> Help
  _unknownCommand -> Unknown args
