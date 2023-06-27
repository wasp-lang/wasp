{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.Plan
  ( Plan (..),
    Entity (..),
    Operation (..),
    Page (..),
    generatePlan,
    PlanRule,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.List (find, intercalate, isPrefixOf)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import qualified Text.Parsec as Parsec
import Wasp.AI.CodeAgent (CodeAgent)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionStartMarkerLine)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import qualified Wasp.Psl.Parser.Model as Psl.Parser
import qualified Wasp.Util.Aeson as Util.Aeson

-- | Additional rule to follow while generating plan.
type PlanRule = String

generatePlan :: Wasp.AI.GenerateNewProject.Common.NewProjectDetails -> [PlanRule] -> CodeAgent Plan
generatePlan newProjectDetails planRules = do
  queryChatGPTForJSON defaultChatGPTParams chatMessages
    >>= fixPlanIfNeeded
  where
    chatMessages =
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = planPrompt}
      ]
    appName = T.pack $ _projectAppName newProjectDetails
    appDesc = T.pack $ _projectDescription newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    waspFileExamplePrompt = Prompts.waspFileExample
    rulesText = T.pack . unlines $ "Instructions you must follow while generating plan:" : map (" - " ++) planRules
    planPrompt =
      [trimming|
        ${basicWaspLangInfoPrompt}

        ${waspFileExamplePrompt}

        We are looking for a plan to build a new Wasp app (description at the end of prompt).

        ${rulesText}

        Plan is represented as JSON. Here is what a plan should look like:

        {
          "entities": [{
            "entityName": "EntityName",
            "entityBodyPsl": "id Int @id \n name String"
          }],
          "actions": [{
            "opName": "ActionName",
            "opFnPath": "@server/{filename}.js",
            "opDesc": "description of what this action does"
          }],
          "queries": [{
            "opName": "QueryName",
            "opFnPath": "@server/{filename}.js",
            "opDesc": "description of what this query does"
          }],
          "pages": [{
            "pageName": "ExamplePage",
            "componentPath": "@client/{ComponentName}.jsx",
            "routeName: "ExampleRoute",
            "routePath": "/url/of/page",
            "pageDesc": "description of what this page does",
          }]
        }

        We will later use this plan to implement all of these parts of Wasp app,
        so make sure descriptions are detailed enough to guide implementing them.

        Typically, plan will have at least one query, at least one action, at least one page, and at
        least two entities. It will very likely though have more than one of each.

        Please, respond ONLY with a valid JSON that is a plan.
        There should be no other text in the response.

        ${appDescriptionStartMarkerLine}

        App name: ${appName}
        ${appDesc}
      |]

    fixPlanIfNeeded :: Plan -> CodeAgent Plan
    fixPlanIfNeeded plan = do
      let issues =
            checkPlanForEntityIssues plan
              <> checkPlanForOperationIssues Query plan
              <> checkPlanForOperationIssues Action plan
              <> checkPlanForPageIssues plan
      if null issues
        then return plan
        else do
          let issuesText = T.pack $ intercalate "\n" $ (<> " - ") <$> issues
          queryChatGPTForJSON defaultChatGPTParams $
            chatMessages
              <> [ ChatMessage {role = Assistant, content = Util.Aeson.encodeToText plan},
                   ChatMessage
                     { role = User,
                       content =
                         [trimming|
                           I found following potential issues with the plan that you made:

                           ${issuesText}

                           Please improve the plan with regard to these issues and any other potential issues that you find.

                           Respond ONLY with a valid JSON that is a plan.
                           There should be no other text or explanations in the response.
                         |]
                     }
                 ]

checkPlanForEntityIssues :: Plan -> [String]
checkPlanForEntityIssues plan =
  checkNumEntities
    <> checkUserEntity
    <> concatMap checkIfEntityPSLCompiles (entities plan)
  where
    checkNumEntities =
      let numEntities = length (entities plan)
       in if numEntities < 2
            then
              [ "There is only " <> show numEntities <> " entities in the plan,"
                  <> " I would expect at least 2 or more."
              ]
            else []

    checkUserEntity =
      case find ((== "User") . entityName) (entities plan) of
        Just _userEntity -> [] -- TODO: I could check here if it contains correct fields.
        Nothing -> ["'User' entity is missing."]

    checkIfEntityPSLCompiles entity =
      case parsePslBody (entityBodyPsl entity) of
        Left parseError ->
          [ "Failed to parse PSL body of entity '" <> entityName entity <> "': "
              <> show parseError
          ]
        Right _ -> []

    parsePslBody = Parsec.parse Psl.Parser.body ""

checkPlanForOperationIssues :: OperationType -> Plan -> [String]
checkPlanForOperationIssues opType plan =
  checkNumOperations
    <> concatMap checkOperationFnPath operations
  where
    operations = caseOnOpType queries actions $ plan

    checkNumOperations =
      let numOps = length operations
       in if numOps < 2
            then
              [ "There is only " <> show numOps <> " " <> caseOnOpType "queries" "actions" <> " in the plan,"
                  <> " I would expect at least 1 or more."
              ]
            else []

    checkOperationFnPath op =
      if not ("@server" `isPrefixOf` opFnPath op)
        then
          [ "fn path of " <> caseOnOpType "query" "action" <> " '" <> opName op
              <> "' must start with '@server'."
          ]
        else []

    caseOnOpType :: a -> a -> a
    caseOnOpType queryCase actionCase = case opType of Query -> queryCase; Action -> actionCase

checkPlanForPageIssues :: Plan -> [String]
checkPlanForPageIssues plan =
  checkNumPages
    <> concatMap checkPageComponentPath (pages plan)
  where
    checkNumPages =
      let numPages = length (pages plan)
       in if numPages < 2
            then
              [ "There is only " <> show numPages <> " pages in the plan,"
                  <> " I would expect at least 1 or more."
              ]
            else []

    checkPageComponentPath page =
      if not ("@client" `isPrefixOf` componentPath page)
        then
          [ "component path of page '" <> pageName page <> "' must start with '@client'."
          ]
        else []

-- TODO: Alternative idea is to give quite more autonomy and equip it with tools (functions) it
--   needs to build correct context, and then let it drive itself completely on its own. So we give
--   it app description, and then let it just go for it -> it would be making freeform plans for
--   itself and executing them, until it is done. We could make following functions available to it:
--     1. For querying Wasp docs. Maybe some specialized functions for things we know it will need a
--        lot, like getActionDocs, getQueryDocs, getPageDocs, and then one general for the rest. The
--        specialized ones we can really optimize to be super useful. Or maybe we do it like this:
--        we have one function which is `fetchDocsForFeature(feature)`, and another
--        `searchDocs(query)`. We tell it to prefer `fetchDocsForFeature` when it can, but it can
--        also use the `searchDocs` when needed. In `fetchDocsForFeature`, `feature` parameter would
--        be an enum we define, and for every feature we could serve exactly the specific piece of
--        docs, or even pre-prepared stuff just for this purpose. So we have control here, to ensure
--        quality. We can tell it that if implementing Action, it should seriously consider fetching
--        the docs for it, if it doesn't have them in context yet.
--     2. For exploring a rich Wasp example that we provide. List all the files. Read specific file.
--        Maybe even searching through its code?
--     3. For exploring the Wasp app it is currently building. Similar like (2).
--     4. It would also need a function for writing a file! So it can generate code. Sure.
--   We can still start with the skeleton project, to make it easier for it.
--
--   So how would this work then? It would generate a plan (freeform, just text), and then in each
--   step it would execute it + update it.
--
--   Interesting question is, how do we construct the Chat each time -> we will probably want to
--   keep including the previous conversation, including its answers? How much of it, how will we
--   determine that? Do we need to ask it to summarize previous convo, and then include last couple
--   of replies? What about the stuff it fetches for itself, we need to make sure to provide that,
--   so it can use it. This sounds like the tricky part to manage, to figure out when to start
--   dropping stuff. We would probably be always passing the whole conversation so far, but if it
--   grows too long (we can use heuristic to guess number of tokens), we ask it to summarize convo
--   so far and add couple of last messages and continue with that. I believe I read that is how
--   they also do it in their UI.

data Plan = Plan
  { entities :: [Entity],
    queries :: [Operation],
    actions :: [Operation],
    pages :: [Page]
  }
  deriving (Generic, Show)

instance FromJSON Plan

instance ToJSON Plan

data Entity = Entity
  { entityName :: String,
    entityBodyPsl :: String
  }
  deriving (Generic, Show)

instance FromJSON Entity

instance ToJSON Entity

data OperationType = Action | Query

data Operation = Operation
  { opName :: String,
    opFnPath :: String,
    opDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Operation

instance ToJSON Operation

data Page = Page
  { pageName :: String,
    componentPath :: String,
    routeName :: String,
    routePath :: String,
    pageDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Page

instance ToJSON Page
