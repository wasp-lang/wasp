module Wasp.AI.GenerateNewProject.WaspFile
  ( fixWaspFile,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( CodeAgent,
    FileContent (..),
    NewProjectDetails,
    codingChatGPTParams,
    fixingChatGPTParams,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionBlock)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Plan (Plan)
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import Wasp.Analyzer.Parser.Ctx (Ctx (..))
import Wasp.Project.WaspFile.WaspLang (analyzeWaspFileContent)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Util.Aeson as Utils.Aeson

fixWaspFile :: NewProjectDetails -> FilePath -> Plan -> CodeAgent ()
fixWaspFile newProjectDetails waspFilePath plan = do
  currentWaspFileContent <- getFile waspFilePath <&> fromMaybe (error "couldn't find wasp file to fix")

  -- First we do one attempt at fixing wasp file even if there are no compiler errors,
  -- to give chatGPT opportunity to fix some other stuff we mention in the prompt.
  -- Then, we do two more attempts at fixing, but only if there are compiler errors.
  fixedWaspFile <-
    pure (FileContent {fileContent = currentWaspFileContent})
      >>= askChatGptToFixWaspFile EvenIfNoCompileErrors
      >>= askChatGptToFixWaspFile OnlyIfCompileErrors
      >>= askChatGptToFixWaspFile OnlyIfCompileErrors

  writeToFile waspFilePath (const $ fileContent fixedWaspFile)
  where
    askChatGptToFixWaspFile :: ShouldContinueIfCompileErrors -> FileContent -> CodeAgent FileContent
    askChatGptToFixWaspFile shouldContinueIfCompileErrors FileContent {fileContent = wfContent} = do
      compileErrors <- liftIO $ getWaspFileCompileErrors wfContent
      case shouldContinueIfCompileErrors of
        OnlyIfCompileErrors | null compileErrors -> return $ FileContent {fileContent = wfContent}
        _otherwise ->
          queryChatGPTForJSON
            (fixingChatGPTParams $ codingChatGPTParams newProjectDetails)
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

            =============

            ${waspFileExamplePrompt}

            =============

            We are together building a new Wasp app (description at the end of prompt).
            Here is a wasp file that we generated together so far:

            ```wasp
            ${currentWaspFileContent}
            ```

            Here is the plan that we generated it based on:
            ${planJSON}

            This file likely has some mistakes: let's fix it!

            ${compileErrorsText}

            Some common mistakes to look for:
              - Don't by accident put all the declarations under `app {...}`! `route`, `page`, `job`, `action`, `query`, those are all standalone and don't go inside of `app`. Assume all the declarations are at the right level in the file and keep that as it is, don't change that.
              - Don't remove any newlines. Sometimes you return the file without newlines, don't do that.
              - Using non-default imports in page components.
                  In a Wasp `page` declaration, the `component` should always use the "default import" JS syntax.
                  Instead of `component: import { PageName } from ...`, it should 
                  always be `component: import PageName from ...`.
                  Fix these by identifying named imports in wasp `page` declarations, and replacing 
                  them with default imports. This only relates to wasp `page` declarations, other parts 
                  of a Wasp file do not have to use the default imports.

              - Missing ',' between dictionary entries, for example before `entities` field in action/query.
                Fix these by adding missing ','.
                For example, the following is missing ',' after the component field:
                ```wasp
                  page ExamplePage {
                    component: import ExamplePage from "@src/pages/ExamplePage" // <- missing ','
                    authRequired: true
                  }
                ```
              - "TODO" comments or "..." that should be replaced with actual implementation.
                Fix these by replacing them with actual implementation.
              - Strings in Wasp must use double quotes, not single quotes.
              - Value of `fn:` field in `query` or `action` not having correct import syntax,
                for example it might have invalid syntax, e.g. `fn: @src/actions`.
                Fix these by replacing it with correct syntax, e.g. `fn: import { actionName } from "@src/actions"`.
              - I noticed that you sometimes by accident add redundant "}" at the end of the Wasp file while fixing it.
                Be careful not to do that.
              - We are using SQLite as a database for Prisma, so we can't use scalar arrays in PSL, like `String[]`,
                as those are not supported in SQLite. We can of course normally use arrays of other models, like `Task[]`.
              - Wasp file should not contain any `entity` declarations! That was ok in older Wasp versions, but now entities are described in prisma.schema file, and not in wasp file.

            With this in mind, generate a new, fixed wasp file.
            Try not to do big changes like changing names, removing/adding declarations and similar, those are usually correct, focus more on obvious, smaller errors.
            Don't touch `app` declaration, `Login` page, and `Signup` page.
            Do actual fixes, don't leave comments with "TODO"!
            Make extra sure to fix compiler errors, if there are any.
            Please respond ONLY with a valid JSON of the format { fileContent: string }.
            There should be no other text in your response. Don't wrap content with the "```" code delimiters.
            Don't ommit newlines from the code.

            ${appDescriptionBlockText}
          |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    waspFileExamplePrompt = Prompts.waspFileExample
    planJSON = Utils.Aeson.encodeToText plan

data ShouldContinueIfCompileErrors = OnlyIfCompileErrors | EvenIfNoCompileErrors

getWaspFileCompileErrors :: Text -> IO [String]
getWaspFileCompileErrors waspSource =
  -- TODO: analyzeWaspFileContent should receive the Prisma Schema AST
  analyzeWaspFileContent (Psl.Schema.Schema []) (T.unpack waspSource)
    <&> either (map showCompileError) (const [])
  where
    showCompileError (errMsg, Ctx {ctxSourceRegion = loc}) = show loc <> ": " <> errMsg
