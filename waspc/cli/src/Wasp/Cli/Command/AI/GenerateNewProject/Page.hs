{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.AI.GenerateNewProject.Page
  ( generateAndWritePage,
    Page (..),
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
    writeToWaspFileEnd,
  )
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.Cli.Command.AI.GenerateNewProject.Entity (entityPlanToWaspDecl)
import Wasp.Cli.Command.AI.GenerateNewProject.Operation (Operation)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as Plan
import Wasp.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))

generateAndWritePage ::
  NewProjectDetails -> FilePath -> [Plan.Entity] -> [Operation] -> [Operation] -> Plan.Page -> CodeAgent Page
generateAndWritePage newProjectDetails waspFilePath entityPlans actions queries pPlan = do
  page <- generatePage newProjectDetails entityPlans actions queries pPlan
  writePageToJsFile page
  writePageToWaspFile waspFilePath page
  writeToLog $ T.pack $ "Generated page: " <> Plan.pageName pPlan
  return page

generatePage :: NewProjectDetails -> [Plan.Entity] -> [Operation] -> [Operation] -> Plan.Page -> CodeAgent Page
generatePage newProjectDetails entityPlans actions queries pPlan = do
  impl <- queryChatGPTForJSON defaultChatGPTParams chatMessages
  return Page {pageImpl = impl, pagePlan = pPlan}
  where
    chatMessages =
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = planPrompt}
      ]

    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    appDescriptionBlock = Prompts.appDescriptionBlock newProjectDetails

    pageName = T.pack $ Plan.pageName pPlan
    componentPath = T.pack $ Plan.componentPath pPlan
    routePath = T.pack $ Plan.routePath pPlan
    pageDesc = T.pack $ Plan.pageDesc pPlan

    entityDecls = T.intercalate "\n\n" $ entityPlanToWaspDecl <$> entityPlans
    -- I should also include info about which args each action has!
    actionsInfo = error "TODO"
    -- I should also include info about which args each query has!
    queriesInfo = error "TODO"

    planPrompt =
      [trimming|
        ${basicWaspLangInfoPrompt}

        ${pageDocPrompt}

        We are implementing a Wasp app (check bottom for description).

        This app has following entities:
        ${entityDecls}

        This app has following actions:
        ${actionsInfo}

        This app has following queries:
        ${queriesInfo}

        Let's now implement the following Wasp page:
         - name: ${pageName}
         - component: ${componentPath}
         - routePath: ${routePath}
         - description: ${pageDesc}

        Please, respond ONLY with a valid JSON, of following format:
        { "pageWaspDecl": "route ${pageName}Route { ... }\npage ${pageName}Page {\n ... }",
          "pageJsImpl": "JS imports + React component implementing the page.",
        }
        There should be no other text in the response.

        ${appDescriptionBlock}
      |]

    pageDocPrompt =
      [trimming|
        Page is implemented via Wasp declaration and corresponding NodeJS implementation.

        Example of Wasp declaration:

        ```wasp
        route ExampleRoute { path: "/", to: ExamplePage }
        page ExamplePage {
          component: import { ExamplePage } from "@client/ExamplePage.jsx",
          authRequired: true
        }
        ```

        Example of ReactJS implementation:

        ```jsx
           TODO
        ```

        TODO: Maybe this part below is not needed, if we put it in example instead.
        You can import actions and queries like this:
        import queryName from "@wasp/queries/queryName";
        and use it with import { useQuery } from "@wasp/queries";

        Auth helpers:
        import logout from "@wasp/auth/logout";
      |]

writePageToJsFile :: Page -> CodeAgent ()
writePageToJsFile page =
  writeToFile path $ (<> "\n\n" <> jsImpl) . fromMaybe ""
  where
    path = resolvePath $ Plan.componentPath $ pagePlan page
    jsImpl = T.pack $ pageJsImpl $ pageImpl page
    pathPrefix = "@client/"
    resolvePath p | pathPrefix `isPrefixOf` p = "src/" <> drop (length ("@" :: String)) p
    resolvePath _ = error "path incorrectly formatted, should start with " <> pathPrefix <> "."

writePageToWaspFile :: FilePath -> Page -> CodeAgent ()
writePageToWaspFile waspFilePath page =
  writeToWaspFileEnd waspFilePath $ "\n" <> T.pack (pageWaspDecl $ pageImpl page)

data Page = Page
  { pageImpl :: PageImpl,
    pagePlan :: Plan.Page
  }
  deriving (Show)

data PageImpl = PageImpl
  { pageWaspDecl :: String,
    pageJsImpl :: String
  }
  deriving (Generic, Show)

instance FromJSON PageImpl
