{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.AI.GenerateNewProject.Action
  ( generateAndWriteAction,
    Action,
  )
where

import Data.Aeson (FromJSON)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent, writeToFile, writeToLog)
import Wasp.Cli.Command.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts (appDescriptionStartMarkerLine)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.Cli.Command.AI.GenerateNewProject.Entity (entityPlanToWaspDecl)
import Wasp.Cli.Command.AI.GenerateNewProject.Plan (Plan)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as Plan
import Wasp.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))

generateAndWriteAction :: NewProjectDetails -> FilePath -> Plan -> Plan.Action -> CodeAgent Action
generateAndWriteAction newProjectDetails waspFilePath plan actPlan = do
  action <- generateAction newProjectDetails (Plan.entities plan) actPlan
  writeActionToJsFile action
  writeActionToWaspFile waspFilePath action
  writeToLog $ "Generated action: " <> T.pack (Plan.actionName actPlan)
  return action

generateAction :: NewProjectDetails -> [Plan.Entity] -> Plan.Action -> CodeAgent Action
generateAction newProjectDetails entityPlans plan = do
  impl <- queryChatGPTForJSON defaultChatGPTParams chatMessages
  return Action {actionImpl = impl, actionPlan = plan}
  where
    chatMessages =
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = planPrompt}
      ]
    appName = T.pack $ _projectAppName newProjectDetails
    appDesc = T.pack $ _projectDescription newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    actionName = T.pack $ Plan.actionName plan
    actionFnPath = T.pack $ Plan.actionFnPath plan
    actionDesc = T.pack $ Plan.actionDesc plan
    entityDecls = T.intercalate "\n\n" $ entityPlanToWaspDecl <$> entityPlans
    planPrompt =
      [trimming|
        ${basicWaspLangInfoPrompt}

        ${actionDocPrompt}

        We are implementing a Wasp app (check bottom for description).

        This app has following entities:
        ${entityDecls}

        Let's now implement the following Wasp action:
         - name: ${actionName}
         - fn: ${actionFnPath}
         - description: ${actionDesc}

        Please, respond ONLY with a valid JSON, of following format:
        { "actionWaspDecl": "action ${actionName} {\n ... }",
          "actionJsImpl": "export const {$actionName} ... ",
          "actionJsImports": "import foo from \"bar.js\"\n ...""
        }
        "waspDeclaration" and "jsImplementation" are required, "jsImports" you can skip if none are needed.
        There should be no other text in the response.

        ${appDescriptionStartMarkerLine}

        App name: ${appName}
        ${appDesc}
      |]
    actionDocPrompt =
      [trimming|
        Action is implemented via Wasp declaration and corresponding NodeJS implementation.

        Example of Wasp declaration:

        ```wasp
        action updateTaskIsDone {
          fn: import { updateTaskIsDone } from "@server/taskActions.js",
          entities: [Task] // Entities that action uses.
        }
        ```

        Example of NodeJS implementation:

        ```js
        import HttpError from '@wasp/core/HttpError.js'

        export const updateTaskIsDone = (args, context) => {
          if (!context.user) { throw new HttpError(403) }

          return context.entities.Task.update({ // prisma object
            where: { args.id },
            data: { args.isDone }
          })
        }
        ```

        Action can then be easily called from the client, via Wasp's RPC mechanism.
      |]

writeActionToJsFile :: Action -> CodeAgent ()
writeActionToJsFile action =
  -- TODO: An issue we have here is that if other action already did the same import,
  --   we don't know and we import it again.
  --   One thing we can do it supply chatGPT with a list of imports that are already there.
  --   Second thing we can do is to look for same lines at the start of the file, but that sounds fragile.
  --   Maybe best to read and pass previous imports (we would have to do that above somewhere).
  --   Or even the whole file? Hmmmmm.
  writeToFile path $
    ((jsImports <> "\n") <>) . (<> "\n\n" <> jsImpl) . (fromMaybe "")
  where
    path = resolvePath $ Plan.actionFnPath $ actionPlan action
    jsImpl = T.pack $ actionJsImpl $ actionImpl action
    jsImports = T.pack $ actionJsImports $ actionImpl action
    resolvePath p | "@server/" `isPrefixOf` p = "src/server/" <> drop (length ("@server/" :: String)) p
    resolvePath _ = error "path incorrectly formatted, should start with @server."

writeActionToWaspFile :: FilePath -> Action -> CodeAgent ()
writeActionToWaspFile waspFilePath action =
  writeToFile waspFilePath $
    (<> "\n\n" <> waspDeclCode) . fromMaybe (error "wasp file shouldn't be empty")
  where
    waspDeclCode = T.pack $ actionWaspDecl $ actionImpl action

data Action = Action
  { actionImpl :: ActionImpl,
    actionPlan :: Plan.Action
  }
  deriving (Show)

data ActionImpl = ActionImpl
  { actionWaspDecl :: String,
    actionJsImpl :: String,
    actionJsImports :: String
  }
  deriving (Generic, Show)

instance FromJSON ActionImpl
