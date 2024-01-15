{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.PageComponentFile
  ( fixPageComponent,
    fixImportsInPageComponentFile,
    -- NOTE: Exports below are exported only for testing!
    getPageComponentFileContentWithFixedImports,
    partitionComponentFileByImports,
    getImportedNamesFromImport,
    getAllPossibleWaspJsClientImports,
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
import Text.Printf (printf)
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
import Wasp.AI.GenerateNewProject.Operation (Operation)
import qualified Wasp.AI.GenerateNewProject.Operation as Operation
import Wasp.AI.GenerateNewProject.Page (makePageDocPrompt)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan
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

-- | Given a list of all operations in the app, it returns a list of all possible @@wasp imports
-- that a Page could import. Those are imports for the specified operations, but also some general
-- imports like login/logouts, hooks, ... .
-- Each entry in the returned map is one possible @@wasp import, where key is imported symbol
-- while import statement is the value.
getAllPossibleWaspJsClientImports :: [Operation] -> M.Map String String
getAllPossibleWaspJsClientImports operations = M.fromList $ possibleUnchangingImports ++ map makeOperationImport operations
  where
    possibleUnchangingImports :: [(String, String)]
    possibleUnchangingImports =
      [ ("logout", "import logout from '@wasp/auth/logout';"),
        ("useAuth", "import useAuth from '@wasp/auth/useAuth';"),
        ("useQuery", "import { useQuery } from '@wasp/queries';"),
        ("useAction", "import { useAction } from '@wasp/actions';")
      ]

    makeOperationImport :: Operation -> (String, String)
    makeOperationImport operation = (opName, opImport)
      where
        opImport :: String
        opImport = printf "import %s from '@wasp/%s/%s';" opName opType opName

        opName :: String
        opName = Plan.opName $ Operation.opPlan operation

        opType :: String
        opType = case Operation.opType operation of
          Operation.Action -> "actions"
          Operation.Query -> "queries"

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
            - Make sure that the component is exported as a default export.

          With this in mind, generate a new, fixed React component (${pageComponentPathText}).
          Do actual fixes, don't leave comments with "TODO"!
          Please respond ONLY with a valid JSON of the format { pageComponentImpl: string }.
          There should be no other text in your response. Don't wrap content with the "```" code delimiters.

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
