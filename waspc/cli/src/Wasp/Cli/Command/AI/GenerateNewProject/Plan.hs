{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.AI.GenerateNewProject.Plan
  ( Plan (..),
    Entity (..),
    Query (..),
    Action (..),
    Page (..),
    generatePlan,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent, queryChatGPT)
import Wasp.Cli.Command.AI.GenerateNewProject.Common (NewProjectDetails (_projectAppName, _projectDescription))
import Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts (appDescriptionStartMarkerLine)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.OpenAI.ChatGPT (ChatGPTParams (..), ChatMessage (..), ChatRole (..), Model (..))

generatePlan :: NewProjectDetails -> CodeAgent Plan
generatePlan newProjectDetails = do
  responseJSONText <- naiveTrimJSON <$> queryChatGPT chatGPTParams chatMessages
  case Aeson.eitherDecode $ textToLazyBS responseJSONText of
    Right plan -> return plan
    Left _errMsg ->
      -- TODO: Handle this better. Write to log, to let user know.
      --   Consider sending response back to chatGPT and ask it to fix it.
      error "Failed to parse plan"
  where
    -- TODO: Try configuring temperature.
    -- TODO: Make sure we have max_tokens set to high enough.
    chatGPTParams = ChatGPTParams {_model = GPT_3_5_turbo_16k, _temperature = Just 1.0}
    chatMessages =
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = planPrompt}
      ]
    appName = T.pack $ _projectAppName newProjectDetails
    appDesc = T.pack $ _projectDescription newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    waspFileExamplePrompt = Prompts.waspFileExample
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

        Make sure to generate at least one page with routePath "/".

        We will later use this plan to implement all of these parts of Wasp app.

        Please, respond ONLY with a valid JSON that is a plan.
        There should be no other text in the response.

        ${appDescriptionStartMarkerLine}

        App name: ${appName}
        ${appDesc}
      |]

-- TODO: Alternative approach idea: have it generate main.wasp file first, with comments above stuff
--   with instructions for itself. Then, in the next step, have it generate a plan based on that
--   main.wasp file. The idea is that this way it will have more freedom + might be able to act more
--   holistically.

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

-- | Given a text containing a single instance of JSON and some text around it but no { or }, trim
-- it until just JSON is left.
-- Examples
--   naiveTrimJson "some text { \"a\": 5 } yay" == "{\"a\": 5 }"
--   naiveTrimJson "some {text} { \"a\": 5 }" -> won't work correctly.
naiveTrimJSON :: Text -> Text
naiveTrimJSON textContainingJson =
  T.reverse . T.dropWhile (/= '}') . T.reverse . T.dropWhile (/= '{') $ textContainingJson

textToLazyBS :: Text -> ByteString
textToLazyBS = TLE.encodeUtf8 . TL.fromStrict
