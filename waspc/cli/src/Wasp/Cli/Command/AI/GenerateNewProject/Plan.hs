{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.AI.GenerateNewProject.Plan
  ( Plan (..),
    Entity (..),
    Query (..),
    Action (..),
    Page (..),
    generatePlan,
    PlanRule,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent)
import Wasp.Cli.Command.AI.GenerateNewProject.Common
  ( AuthProvider (UsernameAndPassword),
    NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts (appDescriptionStartMarkerLine)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))

-- | Additional rule to follow while generating plan.
type PlanRule = String

generatePlan :: NewProjectDetails -> [PlanRule] -> CodeAgent Plan
generatePlan newProjectDetails planRules = do
  queryChatGPTForJSON defaultChatGPTParams chatMessages
  where
    chatMessages =
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = planPrompt}
      ]
    appName = T.pack $ _projectAppName newProjectDetails
    appDesc = T.pack $ _projectDescription newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    waspFileExamplePrompt = Prompts.waspFileExample
    rulesText = T.pack . unlines $ "Rules:" : map (" - " ++) planRules
    planPrompt =
      [trimming|
        ${basicWaspLangInfoPrompt}

        ${waspFileExamplePrompt}

        We are looking for a plan to build a new Wasp app (description at the end of prompt).

        Plan is represented as JSON. Here is what a plan should look like:

        {
          "entities": [{
            "entityName": "EntityName",
            "entityBodyPsl": "id Int @id\\nname String"
          }],
          "actions": [{
            "actionName": "ActionName",
            "actionFnPath": "@server/{filename}.js",
            "actionDesc": "description of what this action does"
          }],
          "queries": [{
            "queryName": "QueryName",
            "queryFnPath": "@server/{filename}.js",
            "queryDesc": "description of what this query does"
          }],
          "pages": [{
            "pageName": "PageName",
            "componentPath": "@client/{ComponentName}.jsx",
            "routePath": "/url/of/page",
            "pageDesc": "description of what this page does",
          }]
        }

        ${rulesText}

        We will later use this plan to implement all of these parts of Wasp app,
        so make sure descriptions are detailed enough to guide implementing them.

        Please, respond ONLY with a valid JSON that is a plan.
        There should be no other text in the response.

        ${appDescriptionStartMarkerLine}

        App name: ${appName}
        ${appDesc}
      |]

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
    queries :: [Query],
    actions :: [Action],
    pages :: [Page]
  }
  deriving (Generic, Show)

instance FromJSON Plan

data Entity = Entity
  { entityName :: String,
    entityBodyPsl :: String
  }
  deriving (Generic, Show)

instance FromJSON Entity

data Query = Query
  { queryName :: String,
    queryFnPath :: String,
    queryDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Query

data Action = Action
  { actionName :: String,
    actionFnPath :: String,
    actionDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Action

data Page = Page
  { pageName :: String,
    componentPath :: String,
    routePath :: String,
    pageDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Page
