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
  currentWaspFileContent <- getFile waspFilePath <&> fromMaybe (error "couldn't find wasp file to fix")

  -- First we do one attempt at fixing wasp file even if there are no compiler errors,
  -- to give chatGPT opportunity to fix some other stuff we mention in the prompt.
  -- Then, we do two more attempts at fixing, but only if there are compiler errors.
  fixedWaspFile <-
    pure (WaspFile {waspFileContent = currentWaspFileContent})
      >>= askChatGptToFixWaspFile EvenIfNoCompileErrors
      >>= askChatGptToFixWaspFile OnlyIfCompileErrors
      >>= askChatGptToFixWaspFile OnlyIfCompileErrors

  writeToFile waspFilePath (const $ waspFileContent fixedWaspFile)
  where
    askChatGptToFixWaspFile :: ShouldContinueIfCompileErrors -> WaspFile -> CodeAgent WaspFile
    askChatGptToFixWaspFile shouldContinueIfCompileErrors WaspFile {waspFileContent = wfContent} = do
      compileErrors <- liftIO $ getWaspFileCompileErrors wfContent
      case shouldContinueIfCompileErrors of
        OnlyIfCompileErrors | null compileErrors -> return $ WaspFile {waspFileContent = wfContent}
        _otherwise ->
          queryChatGPTForJSON
            defaultChatGPTParams
            [ ChatMessage {role = System, content = Prompts.systemPrompt},
              ChatMessage {role = User, content = fixWaspFilePrompt wfContent compileErrors}
            ]

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
              - If two entities are in a relation, make sure that they both have a field that references the other entity.
              - If an entity has a field that references another entity (e.g. location), make sure to include @relation directive on that field.
              - If an entity references another entity, make sure the ID field (e.g. locationId) of the referenced entity is also included.

                ```
                entity Location {=psl
                  id Int @id @default(autoincrement())
                  latitude Float
                  longitude Float
                  weathers Weather[]
                psl=}

                entity Weather {=psl
                  id Int @id @default(autoincrement())
                  temperature Float
                  description String
                  location Location @relation(fields: [locationId], references: [id])
                  locationId Int
                psl=}
                ```

            With this in mind, generate a new, fixed wasp file.
            Do actual fixes, don't leave comments with "TODO"!
            Make extra sure to fix compiler errors, if there are any.
            Please respond ONLY with a valid JSON of the format { waspFileContent: string }.
            There should be no other text in your response. Don't wrap content with the "```" code delimiters.

            ${appDescriptionBlockText}
          |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    waspFileExamplePrompt = Prompts.waspFileExample

data ShouldContinueIfCompileErrors = OnlyIfCompileErrors | EvenIfNoCompileErrors

getWaspFileCompileErrors :: Text -> IO [String]
getWaspFileCompileErrors waspSource =
  analyzeWaspFileContent (T.unpack waspSource)
    <&> either (map showCompileError) (const [])
  where
    showCompileError (errMsg, Ctx {ctxSourceRegion = loc}) = show loc <> ": " <> errMsg

data WaspFile = WaspFile
  { waspFileContent :: Text
  }
  deriving (Generic, Show)

instance FromJSON WaspFile

instance ToJSON WaspFile
