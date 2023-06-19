{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.AI.GenerateNewProject.Operation
  ( generateAndWriteOperation,
    Operation (..),
    OperationType (..),
    OperationImpl (..),
  )
where

import Data.Aeson (FromJSON)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, writeToFile, writeToLog)
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import Wasp.Cli.Command.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
    writeToWaspFileEnd,
  )
import Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts (appDescriptionStartMarkerLine)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.Cli.Command.AI.GenerateNewProject.Entity (entityPlanToWaspDecl)
import Wasp.Cli.Command.AI.GenerateNewProject.Plan (Plan)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as Plan

generateAndWriteOperation :: OperationType -> NewProjectDetails -> FilePath -> Plan -> Plan.Operation -> CodeAgent Operation
generateAndWriteOperation operationType newProjectDetails waspFilePath plan operationPlan = do
  operation <- generateOperation operationType newProjectDetails (Plan.entities plan) operationPlan
  writeOperationToJsFile operation
  writeOperationToWaspFile waspFilePath operation
  writeToLog $ T.pack $ "Generated " <> show operationType <> ": " <> Plan.opName operationPlan
  return operation

generateOperation :: OperationType -> NewProjectDetails -> [Plan.Entity] -> Plan.Operation -> CodeAgent Operation
generateOperation operationType newProjectDetails entityPlans operationPlan = do
  impl <- queryChatGPTForJSON defaultChatGPTParams chatMessages
  return Operation {opImpl = impl, opPlan = operationPlan, opType = operationType}
  where
    chatMessages =
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = planPrompt}
      ]
    appName = T.pack $ _projectAppName newProjectDetails
    appDesc = T.pack $ _projectDescription newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    operationName = T.pack $ Plan.opName operationPlan
    operationFnPath = T.pack $ Plan.opFnPath operationPlan
    operationDesc = T.pack $ Plan.opDesc operationPlan
    operationTypeText = T.pack $ show operationType
    entityDecls = T.intercalate "\n\n" $ entityPlanToWaspDecl <$> entityPlans
    planPrompt =
      [trimming|
        ${basicWaspLangInfoPrompt}

        ${operationDocPrompt}

        We are implementing a Wasp app (check bottom for description).

        This app has following entities:
        ${entityDecls}

        Let's now implement the following Wasp ${operationTypeText}:
         - name: ${operationName}
         - fn: ${operationFnPath}
         - description: ${operationDesc}

        Please, respond ONLY with a valid JSON, of following format:
        { "opWaspDecl": "${operationTypeText} ${operationName} {\n ... }",
          "opJsImpl": "export const {$operationName} ... ",
          "opJsImports": "import foo from \"bar.js\"\n ...""
        }
        "waspDeclaration" and "jsImplementation" are required, "jsImports" you can skip if none are needed.
        There should be no other text in the response.

        ${appDescriptionStartMarkerLine}

        App name: ${appName}
        ${appDesc}
      |]

    operationDocPrompt = case operationType of
      Action -> actionDocPrompt
      Query -> queryDocPrompt

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
          if (!context.user) { throw new HttpError(403) } // If user needs to be authenticated.

          return context.entities.Task.update({ // prisma object
            where: { args.id },
            data: { args.isDone }
          })
        }
        ```

        Action can then be easily called from the client, via Wasp's RPC mechanism.
      |]

    queryDocPrompt =
      [trimming|
        Query is implemented via Wasp declaration and corresponding NodeJS implementation.
        It is important that Query doesn't do any mutations, be it on the server or external world.

        Example of Wasp declaration:

        ```wasp
        query fetchFilteredTasks {
          fn: import { getFilteredTasks } from "@server/taskQueries.js",
          entities: [Task] // Entities that query uses.
        }
        ```

        Example of NodeJS implementation:

        ```js
        import HttpError from '@wasp/core/HttpError.js'

        export const getFilteredTasks = async (args, context) => {
          if (!context.user) { throw new HttpError(403) } // If user needs to be authenticated.

          return context.entities.Task.findMany({
            where: { isDone: args.isDone }
          })
        }
        ```

        Query can then be easily called from the client, via Wasp's RPC mechanism.
      |]

writeOperationToJsFile :: Operation -> CodeAgent ()
writeOperationToJsFile operation =
  -- TODO: An issue we have here is that if other operation already did the same import,
  --   we don't know and we import it again.
  --   One thing we can do it supply chatGPT with a list of imports that are already there.
  --   Second thing we can do is to look for same lines at the start of the file, but that sounds fragile.
  --   Maybe best to read and pass previous imports (we would have to do that above somewhere).
  --   Or even the whole file? Hmmmmm.
  writeToFile path $
    ((jsImports <> "\n") <>) . (<> "\n\n" <> jsImpl) . (fromMaybe "")
  where
    path = resolvePath $ Plan.opFnPath $ opPlan operation
    jsImpl = T.pack $ opJsImpl $ opImpl operation
    jsImports = T.pack $ opJsImports $ opImpl operation
    pathPrefix = "@server/"
    resolvePath p | pathPrefix `isPrefixOf` p = "src/" <> drop (length ("@" :: String)) p
    resolvePath _ = error "path incorrectly formatted, should start with " <> pathPrefix <> "."

writeOperationToWaspFile :: FilePath -> Operation -> CodeAgent ()
writeOperationToWaspFile waspFilePath operation =
  writeToWaspFileEnd waspFilePath $ "\n" <> waspDeclCode
  where
    waspDeclCode = T.pack $ opWaspDecl $ opImpl operation

data OperationType = Action | Query

instance Show OperationType where
  show Action = "action"
  show Query = "query"

data Operation = Operation
  { opImpl :: OperationImpl,
    opPlan :: Plan.Operation,
    opType :: OperationType
  }
  deriving (Show)

data OperationImpl = OperationImpl
  { opWaspDecl :: String,
    opJsImpl :: String,
    opJsImports :: String
  }
  deriving (Generic, Show)

instance FromJSON OperationImpl
