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
import Wasp.AI.CodeAgent (getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( CodeAgent,
    NewProjectDetails (..),
    codingChatGPTParams,
    fixingChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionBlock)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Page (makePageDocPrompt)
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))

fixPageComponent :: NewProjectDetails -> FilePath -> FilePath -> CodeAgent ()
fixPageComponent newProjectDetails waspFilePath pageComponentPath = do
  currentWaspFileContent <- fromMaybe (error "couldn't find wasp file") <$> getFile waspFilePath
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  fixedPageComponent <-
    queryChatGPTForJSON
      (fixingChatGPTParams $ codingChatGPTParams newProjectDetails)
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = fixPageComponentPrompt currentWaspFileContent currentPageComponentContent}
      ]
  writeToFile pageComponentPath (const $ pageComponentImpl fixedPageComponent)
  where
    fixPageComponentPrompt currentWaspFileContent currentPageContent =
      [trimming|
          ${basicWaspLangInfoPrompt}

          ===============

          ${pageDocPrompt}

          ===============

          We are together building a new Wasp app (description at the end of prompt).

          Here is our main.wasp file that we generated so far, to provide you with the additional context about the app:
          ```wasp
          ${currentWaspFileContent}
          ```

          Here is a React component (${pageComponentPathText}) containing some frontend code
          that we generated together earlier:
          ```js
          ${currentPageContent}
          ```

          The React component probably has some mistakes: let's fix it!

          Strong guidelines for fixing:
            - Make sure to use only queries and actions that are defined in the Wasp file (listed below)!
            - Use Tailwind CSS to style the page if you didn't.
            - Use <Link /> component from "react-router-dom" to link to other pages where needed.
            - "TODO" comments or "..." that should be replaced with actual implementation.
              Fix these by replacing them with actual implementation.
            - If there are any duplicate imports, make sure to remove them.
            - There might be some invalid JS or JSX syntax -> fix it if there is any.
            - If there are any js imports of local modules (`from "./`, `from "../`),
              remove them and instead add the needed implementation directly in the file we are fixing right now.
            - Remove redundant imports, but don't change any of the remaining ones.
            - Feel free to merge together imports from the same path.
              e.g. if you have `import { useQuery } from 'wasp/client/operations';` and
              `import { useAction } from 'wasp/client/operations';`, you can merge those into
              `import { useQuery, useAction } from 'wasp/client/operations';`.
            - Make sure that the component is exported as a default export.

          With this in mind, generate a new, fixed React component (${pageComponentPathText}).
          Do actual fixes, don't leave comments with "TODO"!
          Please respond ONLY with a valid JSON of the format { pageComponentImpl: string }.
          There should be no other text in your response. Don't wrap content with the "```" code delimiters.
          Don't ommit newlines from the code.

          ${appDescriptionBlockText}
        |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    pageComponentPathText = T.pack pageComponentPath
    pageDocPrompt = makePageDocPrompt

data PageComponent = PageComponent
  { pageComponentImpl :: Text
  }
  deriving (Generic, Show)

instance FromJSON PageComponent
