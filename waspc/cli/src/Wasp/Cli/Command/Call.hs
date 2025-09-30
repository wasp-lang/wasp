module Wasp.Cli.Command.Call where

import qualified Wasp.Cli.Command.CreateNewProject.AI.ArgumentsParser as Command.NewProjectAi
import qualified Wasp.Cli.Command.CreateNewProject.ArgumentsParser as Command.NewProject

data Call
  = New Command.NewProject.NewProjectArgs
  | NewAi Command.NewProjectAi.NewProjectAIArgs
  | Start
  | StartDb Arguments
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
  deriving (Show)

type Arguments = [String]
