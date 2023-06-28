{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.Operation
  ( generateAndWriteOperation,
    Operation (..),
    OperationType (..),
    OperationImpl (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.List (find, intercalate, isInfixOf, isPrefixOf)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, writeToFile, writeToLog)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
    writeToWaspFileEnd,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionStartMarkerLine)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Entity (entityPlanToWaspDecl)
import Wasp.AI.GenerateNewProject.Plan (Plan)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import qualified Wasp.Analyzer.Parser as P
import qualified Wasp.Util.Aeson as Util.Aeson

generateAndWriteOperation :: OperationType -> NewProjectDetails -> FilePath -> Plan -> Plan.Operation -> CodeAgent Operation
generateAndWriteOperation operationType newProjectDetails waspFilePath plan operationPlan = do
  operation <- generateOperation operationType newProjectDetails (Plan.entities plan) operationPlan
  writeOperationToJsFile operation
  writeOperationToWaspFile waspFilePath operation
  writeToLog $ T.pack $ "Generated " <> show operationType <> ": " <> Plan.opName operationPlan
  return operation

generateOperation :: OperationType -> NewProjectDetails -> [Plan.Entity] -> Plan.Operation -> CodeAgent Operation
generateOperation operationType newProjectDetails entityPlans operationPlan = do
  impl <-
    queryChatGPTForJSON defaultChatGPTParams chatMessages
      >>= fixOperationImplIfNeeded
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
        { "opWaspDecl": string,  // Wasp declaration.
          "opJsImpl": string,    // Javascript implementation that will go into ${operationFnPath} file.
          "opJsImports": string  // Javascript imports that will go into ${operationFnPath} file, needed by the JS implementation.
        }

        Example of response:
        { "opWaspDecl": "${operationTypeText} ${operationName} {\n  fn: import { ${operationName} } from \"${operationFnPath}\",\n  entities: [Task]\n}",
          "opJsImpl": "export const {$operationName} = async (args, context) => { ... }",
          "opJsImports": "import HttpError from '@wasp/core/HttpError.js'"
        }
        "opWaspDecl" and "opJsImpl" are required, "opJsImports" you can skip if none are needed.
        There should be no other text in the response, just valid JSON.

        Additional strong guidelines for you to follow:
         - There should typically be at least one entity listed under `entities` field in the wasp declaration.
         - Don't ever put "..." or "//TODO" comments in the wasp declaration (`opWaspDecl`) or js implementation (`opJsImpl`).
           Instead, always write real implementation!
         - Don't import prisma client in the JS imports, it is not needed.
         - In wasp declaration (`opWaspDecl`), make sure to use ',' before `entities:`.
           Also, make sure to use full import statement for `fn:`: `import { getTasks } from "@server/actions.js"`,
           don't provide just the file path.

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

    fixOperationImplIfNeeded :: OperationImpl -> CodeAgent OperationImpl
    fixOperationImplIfNeeded operationImpl = do
      let issues = checkWaspDecl operationImpl <> checkJsImpl operationImpl
      if null issues
        then return operationImpl
        else do
          let issuesText = T.pack $ intercalate "\n" ((" - " <>) <$> issues)
          queryChatGPTForJSON defaultChatGPTParams $
            chatMessages
              <> [ ChatMessage {role = Assistant, content = Util.Aeson.encodeToText operationImpl},
                   ChatMessage
                     { role = User,
                       content =
                         [trimming|
                           I found following potential issues with the ${operationTypeText} that you generated:

                           ${issuesText}

                           Please improve the ${operationTypeText} with regard to these issues and any other potential issues that you find.

                           Respond ONLY with a valid JSON that is an ${operationTypeText}.
                           There should be no other text or explanations in the response.
                         |]
                     }
                 ]

-- TODO: This is quite manual here, checking the AST!
-- Consider instead generating entities during assembling Plan,
-- and then using those to manually construct the wasp decl, so we are
-- sure it is correct and don't have to check it here.
-- Check for number of entities would then go into plan.
checkWaspDecl :: OperationImpl -> [String]
checkWaspDecl operationImpl =
  case P.parseStatements $ opWaspDecl operationImpl of
    Left err -> [show err]
    Right
      P.AST
        { astStmts =
            [P.WithCtx _ (P.Decl _ _ (P.WithCtx _ (P.Dict dictEntries)))]
        } ->
        let entitiesIssues = case find ((== "entities") . fst) dictEntries of
              Nothing -> ["There is no `entities` field."]
              Just ("entities", P.WithCtx _ (P.List entities)) ->
                if null entities
                  then ["There are 0 entities listed, typically there should be at least 1."]
                  else []
              Just _wrongShape -> ["'entities' field is of of wrong shape, it should be a list."]
            fnIssues = case find ((== "fn") . fst) dictEntries of
              Nothing -> ["There is no `fn` field."]
              Just ("fn", P.WithCtx _ (P.ExtImport _name _path)) -> []
              Just _wrongShape -> ["`fn` field is of of wrong shape."]
         in entitiesIssues <> fnIssues
    Right _wrongShape -> ["Operation declaration should be a dictionary."]

checkJsImpl :: OperationImpl -> [String]
checkJsImpl operationImpl =
  let todoIssue =
        if "TODO" `isInfixOf` jsImpl
          then ["Seems like there is a 'TODO' in the js implementation. Replace it with real implementation!"]
          else []
      threeDotsIssue =
        if "..." `isInfixOf` jsImpl
          then ["Seems like there is a '...' in the js implementation. Replace it with real implementation!"]
          else []
   in todoIssue <> threeDotsIssue
  where
    jsImpl = opJsImpl operationImpl

writeOperationToJsFile :: Operation -> CodeAgent ()
writeOperationToJsFile operation =
  -- TODO: An issue we have here is that if other operation already did the same import,
  --   we don't know and we import it again.
  --   One thing we can do it supply chatGPT with a list of imports that are already there.
  --   Second thing we can do is to look for same lines at the start of the file, but that sounds fragile.
  --   Maybe best to read and pass previous imports (we would have to do that above somewhere).
  --   Or even the whole file? Hmmmmm.
  writeToFile path $
    (jsImportsBlock <>) . (<> jsImpl) . maybe "" (<> "\n\n")
  where
    path = resolvePath $ Plan.opFnPath $ opPlan operation
    jsImpl = T.pack $ opJsImpl $ opImpl operation
    jsImportsBlock = T.pack $ maybe "" (<> "\n") $ opJsImports $ opImpl operation
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
  { opImpl :: !OperationImpl,
    opPlan :: !Plan.Operation,
    opType :: !OperationType
  }
  deriving (Show)

data OperationImpl = OperationImpl
  { opWaspDecl :: !String,
    opJsImpl :: !String,
    opJsImports :: !(Maybe String)
  }
  deriving (Generic, Show)

instance FromJSON OperationImpl

instance ToJSON OperationImpl
