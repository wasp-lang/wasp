{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.PageComponentFile
  ( fixPageComponent,
    fixImportsInPageComponentFile,
    -- NOTE: Exports below are exported only for testing!
    getPageComponentFileContentWithFixedImports,
    partitionComponentFileByImports,
    getImportedNamesFromImport,
  )
where

import Control.Arrow (first)
import Data.Aeson (FromJSON)
import Data.List (intercalate, isInfixOf, isPrefixOf, partition, stripPrefix)
import Data.List.Extra (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParamsForFixing,
    queryChatGPTForJSON,
  )
import Wasp.AI.GenerateNewProject.Common.Prompts (appDescriptionBlock)
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Operation (Operation)
import Wasp.AI.GenerateNewProject.Page (getAllPossibleWaspJsClientImports, makePageDocPrompt)
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))
import Wasp.Util (trim)

fixImportsInPageComponentFile :: FilePath -> [Operation] -> [Operation] -> CodeAgent ()
fixImportsInPageComponentFile pageComponentPath queries actions = do
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  let fixedComponentContent = getPageComponentFileContentWithFixedImports currentPageComponentContent allPossibleWaspImports
  writeToFile pageComponentPath (const fixedComponentContent)
  where
    allPossibleWaspImports = getAllPossibleWaspJsClientImports $ queries ++ actions

getPageComponentFileContentWithFixedImports :: Text -> Map String String -> Text
getPageComponentFileContentWithFixedImports pageComponentContent allPossibleWaspImports =
  T.intercalate "\n" [nonWaspImportsText, fixedWaspImportsText, remainingCodeText]
  where
    fixedWaspImportsText = T.pack $ intercalate "\n" $ mapMaybe (`M.lookup` allPossibleWaspImports) importedNames
    nonWaspImportsText = T.pack $ intercalate "\n" nonWaspImports
    remainingCodeText = T.pack $ intercalate "\n" remainingCode
    importedNames = nub $ concatMap getImportedNamesFromImport waspImports
    (waspImports, nonWaspImports, remainingCode) = partitionComponentFileByImports pageComponentContent

-- NOTE: Doesn't work correctly for imports that use `as` keyword!
getImportedNamesFromImport :: String -> [String]
getImportedNamesFromImport =
  nub
    . words
    . map convertSpecialCharToSpace
    . trim
    . removeSuffix "from"
    . trim
    . removePrefix "import"
    . trim
    . takeWhile (not . (`elem` ['"', '\'']))
  where
    convertSpecialCharToSpace char
      | char `elem` [',', '}', '{'] = ' '
      | otherwise = char

    removePrefix prefix = fromJust . stripPrefix prefix

    removeSuffix suffix = reverse . removePrefix (reverse suffix) . reverse

partitionComponentFileByImports :: Text -> ([String], [String], [String])
partitionComponentFileByImports componentContent = (waspImportLines, nonWaspImportLines, "" : remainingCodeLines)
  where
    (waspImportLines, nonWaspImportLines) = partition isWaspImportLine importLines
    (importLines, remainingCodeLines) =
      first cleanUpImportLines $
        span isImportLineOrEmpty $ lines $ T.unpack componentContent

    isImportLineOrEmpty l = let l' = trim l in "import" `isPrefixOf` l' || null l'
    isWaspImportLine = ("@wasp" `isInfixOf`)
    cleanUpImportLines = filter (not . null) . fmap trim

fixPageComponent :: NewProjectDetails -> FilePath -> FilePath -> [Operation] -> [Operation] -> CodeAgent ()
fixPageComponent newProjectDetails waspFilePath pageComponentPath queries actions = do
  currentWaspFileContent <- fromMaybe (error "couldn't find wasp file") <$> getFile waspFilePath
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  fixedPageComponent <-
    queryChatGPTForJSON
      defaultChatGPTParamsForFixing
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

          With this in mind, generate a new, fixed React component (${pageComponentPathText}).
          Do actual fixes, don't leave comments with "TODO"!
          Please respond ONLY with a valid JSON of the format { pageComponentImpl: string }.
          There should be no other text in your response. Don't wrap content with the "```" code delimiters.

          ${appDescriptionBlockText}
        |]
    appDescriptionBlockText = appDescriptionBlock newProjectDetails
    basicWaspLangInfoPrompt = Prompts.basicWaspLangInfo
    pageComponentPathText = T.pack pageComponentPath
    pageDocPrompt = makePageDocPrompt $ queries ++ actions

data PageComponent = PageComponent
  { pageComponentImpl :: Text
  }
  deriving (Generic, Show)

instance FromJSON PageComponent
