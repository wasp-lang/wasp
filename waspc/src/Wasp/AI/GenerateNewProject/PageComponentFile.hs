{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.PageComponentFile
  ( fixPageComponent,
  )
where

import Data.Aeson (FromJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionBlock)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Operation (Operation)
import Wasp.AI.GenerateNewProject.Page (operationInfo, pageDocPrompt)
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))

fixPageComponent :: NewProjectDetails -> FilePath -> FilePath -> [Operation] -> [Operation] -> CodeAgent ()
fixPageComponent newProjectDetails waspFilePath pageComponentPath actions queries = do
  currentWaspFileContent <- fromMaybe (error "couldn't find wasp file") <$> getFile waspFilePath
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  fixedPageComponent <-
    queryChatGPTForJSON
      defaultChatGPTParams
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = fixPageComponentPrompt currentWaspFileContent currentPageComponentContent}
      ]
  writeToFile pageComponentPath (const $ pageComponentImpl fixedPageComponent)
  where
    actionsInfo = T.intercalate "\n" $ (" - " <>) . operationInfo <$> actions
    queriesInfo = T.intercalate "\n" $ (" - " <>) . operationInfo <$> queries

    fixPageComponentPrompt currentWaspFileContent currentPageContent =
      [trimming|
          ${basicWaspLangInfoPrompt}

          ${pageDocPrompt}

          ===============

          We are together building a new Wasp app (description at the end of prompt).

          Here is a wasp file that we generated together so far:
          ```wasp
          ${currentWaspFileContent}
          ```

          Here is a React component (${pageComponentPathText}) containing some frontend code
          that we generated together earlier:
          ```js
          ${currentPageContent}
          ```

          ===============

          The NodeJS file with operations likely has some mistakes: let's fix it!

          Some common mistakes to look for:
            - Make sure to use only queries and actions that are defined in the Wasp file (listed below)
            - You should import queries from "@wasp/queries/{queryName}" like import getTask from '@wasp/queries/getTasks';
            - You should import actions from "@wasp/actions/{actionName}" like import createTask from '@wasp/actions/createTask';
            - Use Tailwind CSS to style the page if you didn't.
            - Use <Link /> component from "react-router-dom" to link to other pages where needed.
            - "TODO" comments or "..." that should be replaced with actual implementation.
              Fix these by replacing them with actual implementation.
            - Duplicate imports. If there are any, make sure to remove them.
            - There might be some invalid JS or JSX syntax -> fix it if there is any.

          Actions:
          ${actionsInfo}

          Queries:
          ${queriesInfo}

          With this in mind, generate a new, fixed React component (${pageComponentPathText}).
          Do actual fixes, don't leave comments with "TODO"!
          Please respond ONLY with a valid JSON of the format { pageComponentImpl: string }.
          There should be no other text in your response. Don't wrap content with the "```" code delimiters.

          ${appDescriptionBlockText}
        |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    pageComponentPathText = T.pack pageComponentPath

data PageComponent = PageComponent
  { pageComponentImpl :: Text
  }
  deriving (Generic, Show)

instance FromJSON PageComponent
