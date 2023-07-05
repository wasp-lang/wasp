{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.OperationsJsFile
  ( fixOperationsJsFile,
  )
where

import Control.Monad (when)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails,
    defaultChatGPTParamsForFixing,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionBlock)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Operation (actionDocPrompt, queryDocPrompt)
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import Wasp.Util.StringDiff (printTwoFiles)

fixOperationsJsFile :: NewProjectDetails -> FilePath -> FilePath -> CodeAgent Text
fixOperationsJsFile newProjectDetails waspFilePath opJsFilePath = do
  currentWaspFileContent <- fromMaybe (error "couldn't find wasp file") <$> getFile waspFilePath
  currentOpJsFileContent <- fromMaybe (error "couldn't find operation js file to fix") <$> getFile opJsFilePath
  -- TODO: Would be great if we could compile it, check for compiler errors and then also provide those.
  --   For that however, we would likely need the whole Wasp file generated on the disk,
  --   with npm dependencies installed, so we skipped it for now.
  fixedOpJsFile <-
    queryChatGPTForJSON
      defaultChatGPTParamsForFixing
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = fixOpJsFilePrompt currentWaspFileContent currentOpJsFileContent}
      ]
  writeToFile opJsFilePath (const $ opJsFileContent fixedOpJsFile)

  when True
    $ trace
      ( "=== Diff between old and new "
          <> opJsFilePath
          <> " operation: ===\n"
          <> printTwoFiles (T.unpack currentOpJsFileContent) (T.unpack $ opJsFileContent fixedOpJsFile)
      )
    $ return ()

  return $ listOfFixes fixedOpJsFile
  where
    fixOpJsFilePrompt currentWaspFileContent currentOpJsFileContent =
      [trimming|
          ${basicWaspLangInfoPrompt}

          ${queryDocPrompt}

          ${actionDocPrompt}

          ===============

          We are together building a new Wasp app (description at the end of prompt).

          Here is a wasp file that we generated together so far:
          ```wasp
          ${currentWaspFileContent}
          ```

          Here is a NodeJS file (${opJsFilePathText}) containing some of the Wasp Operations
          (Queries and/or Actions) that we generated together earlier:
          ```js
          ${currentOpJsFileContent}
          ```

          ===============

          The NodeJS file with operations likely has some mistakes: let's fix it!

          Some common mistakes to look for:
            - "TODO" comments or "..." that should be replaced with actual implementation.
              Fix these by replacing them with actual implementation.
            - Duplicate imports. If there are any, make sure to remove them.
            - Redundant imports of prisma client or of prisma entities. Those imports are not needed -> remove them!

              We are not using PrismaClient directly, we are using it through the context. For example: `context.entities.Task.findMany({})`

            - There might be some invalid JS syntax -> fix it if there is any.
            - If there is some obvious refactoring that could improve code quality, go for it.
            - Use the "arg" variable to get the arguments of the operation.
              For example, if the operation is "createTask", then you can get the arguments like this:
              ```js
              const { name, description } = arg;
              ```

          List all of the fixes first and then generate a new, fixed NodeJS file with operations (${opJsFilePathText}).
          The fixes should be a - prefixed list of lines, each line being a fix.
          Don't do too big changes, like deleting or adding whole functions, focus on smaller things and those listed above.
          DO NOT add new queries / actions to the file, or delete existing ones!
          Do actual fixes, don't leave comments with "TODO"!
          Please respond ONLY with a valid JSON of the format { opJsFileContent: string, listOfFixes: string }.
          There should be no other text in your response. Don't wrap content with the "```" code delimiters.

          ${appDescriptionBlockText}
        |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    opJsFilePathText = T.pack opJsFilePath

data OperationsJsFile = OperationsJsFile
  { opJsFileContent :: Text,
    listOfFixes :: Text
  }
  deriving (Generic, Show)

instance FromJSON OperationsJsFile

instance ToJSON OperationsJsFile
