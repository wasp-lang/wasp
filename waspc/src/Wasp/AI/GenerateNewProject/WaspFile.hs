{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.WaspFile
  ( fixWaspFile,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails,
    defaultChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionBlock)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import Wasp.Analyzer.Parser.Ctx (Ctx (..))
import Wasp.Project.Analyze (analyzeWaspFileContent)

fixWaspFile :: NewProjectDetails -> FilePath -> CodeAgent ()
fixWaspFile newProjectDetails waspFilePath = do
  currentWaspFileContent <- fromMaybe (error "couldn't find wasp file to fix") <$> getFile waspFilePath
  compileErrors <-
    liftIO (analyzeWaspFileContent $ T.unpack currentWaspFileContent)
      <&> either (map showCompileError) (const [])
  fixedWaspFile <-
    queryChatGPTForJSON
      defaultChatGPTParams
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = fixWaspFilePrompt currentWaspFileContent compileErrors}
      ]
  writeToFile waspFilePath (const $ waspFileContent fixedWaspFile)
  where
    fixWaspFilePrompt currentWaspFileContent compileErrors =
      let compileErrorsText =
            T.pack $
              if null compileErrors
                then ""
                else
                  "Compile errors we detected:\n"
                    <> intercalate "\n" ((" - " <>) <$> compileErrors)
       in [trimming|
            ${basicWaspLangInfoPrompt}

            ${waspFileExamplePrompt}

            We are together building a new Wasp app (description at the end of prompt).
            Here is a wasp file that we generated together so far:

            ```wasp
            ${currentWaspFileContent}
            ```

            This file likely has some mistakes: let's fix it!

            ${compileErrorsText}

            Some common mistakes to look for:
              - Missing ',' between dictionary entries, for example before `entities` field in action/query.
                Fix these by adding missing ','.

                For example, the following is missing ',' after the component field:
                ```
                  page MainPage {
                    component: import { MainPage } from "@client/pages/MainPage.jsx" // <- missing ','
                    authRequired: true
                  }
                ```

              - "TODO" comments or "..." that should be replaced with actual implementation.
                Fix these by replacing them with actual implementation.
              - Value of `fn:` field in `query` or `action` not having correct import syntax,
                for example it might have invalid syntax, e.g. `fn: @server/actions.js`.
                Fix these by replacing it with correct syntax, e.g. `fn: import { actionName } from "@server/actions.js"`.
              - Entities having a reference to another entity but that entity doesn't have a reference back to it.
                Fix these by adding a reference back to the entity that is referenced.

            With this in mind, generate a new, fixed wasp file.
            Do actual fixes, don't leave comments with "TODO"!
            Please respond ONLY with a valid JSON of the format { waspFileContent: string }.
            There should be no other text in your response. Don't wrap content with the "```" code delimiters.

            ${appDescriptionBlockText}
          |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    waspFileExamplePrompt = Prompts.waspFileExample
    showCompileError (errMsg, Ctx {ctxSourceRegion = loc}) = show loc <> ": " <> errMsg

data WaspFile = WaspFile
  { waspFileContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON WaspFile

instance ToJSON WaspFile
