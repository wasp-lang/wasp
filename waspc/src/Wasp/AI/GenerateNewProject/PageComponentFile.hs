{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Wasp.AI.GenerateNewProject.PageComponentFile
  ( fixPageComponent,
    fixImportsInPageComponent,
    operations,
    queries,
    actions,
  )
where

import Data.Aeson (FromJSON)
import Data.List (intercalate, isInfixOf, isPrefixOf, partition, stripPrefix)
import Data.List.Extra (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)
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
import Wasp.AI.GenerateNewProject.Operation (Operation (opPlan), OperationImpl (OperationImpl))
import qualified Wasp.AI.GenerateNewProject.Operation as Operation
import Wasp.AI.GenerateNewProject.Page (getAllPossibleImports, makePageDocPrompt)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..), Model (GPT_4), _model)
import Wasp.Util (trim)
import qualified Wasp.AI.CodeAgent as CA
import qualified Data.Text.IO as T.IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

deletePost :: Operation
deletePost = Operation.Operation
  { opImpl =
      OperationImpl
        { opWaspDecl = "",
          opJsImpl = "",
          opJsImports = Just ""
        },
    opPlan =
      Plan.Operation
        { opName = "deletePost",
          opFnPath = "",
          opDesc = ""
        },
    opType = Operation.Action
  }

editPost :: Operation
editPost = Operation.Operation
  { opImpl =
      OperationImpl
        { opWaspDecl = "",
          opJsImpl = "",
          opJsImports = Just ""
        },
    opPlan =
      Plan.Operation
        { opName = "editPost",
          opFnPath = "",
          opDesc = ""
        },
    opType = Operation.Action
  }

createPost :: Operation
createPost = Operation.Operation
  { opImpl =
      OperationImpl
        { opWaspDecl = "",
          opJsImpl = "",
          opJsImports = Just ""
        },
    opPlan =
      Plan.Operation
        { opName = "createPost",
          opFnPath = "",
          opDesc = ""
        },
    opType = Operation.Action
  }

createComment :: Operation
createComment = Operation.Operation
  { opImpl =
      OperationImpl
        { opWaspDecl = "",
          opJsImpl = "",
          opJsImports = Just ""
        },
    opPlan =
      Plan.Operation
        { opName = "createComment",
          opFnPath = "",
          opDesc = ""
        },
    opType = Operation.Action
  }

deleteComment :: Operation
deleteComment = Operation.Operation
  { opImpl =
      OperationImpl
        { opWaspDecl = "",
          opJsImpl = "",
          opJsImports = Just ""
        },
    opPlan =
      Plan.Operation
        { opName = "deleteComment",
          opFnPath = "",
          opDesc = ""
        },
    opType = Operation.Action
  }

getPost :: Operation
getPost =
  Operation.Operation
    { opImpl =
        OperationImpl
          { opWaspDecl = "",
            opJsImpl = "",
            opJsImports = Just ""
          },
      opPlan =
        Plan.Operation
          { opName = "getPost",
            opFnPath = "",
            opDesc = ""
          },
      opType = Operation.Query
    }
  
operations :: [Operation]
operations = [deletePost, editPost, createPost, createComment, deleteComment, getPost]

queries :: [Operation]
queries = [getPost]

actions :: [Operation]
actions = [deletePost, editPost, createPost, createComment, deleteComment]

fixImports :: FilePath -> [Operation] -> [Operation] -> CodeAgent ()
fixImports pageComponentPath queries actions = do
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  let fixedComponentContent = getComponentWithFixedImports currentPageComponentContent importsForNames
  writeToFile pageComponentPath (const fixedComponentContent)
  where
    importsForNames = getWaspImportsForNames $ queries ++ actions

getComponentWithFixedImports :: Text -> Map String String -> Text
getComponentWithFixedImports pageComponentContent importsForNames = fixedComponentContent
  where
    fixedComponentContent = T.intercalate "\n" [nonWaspImportsText, fixedWaspImportsText, remainingCodeText]
    fixedWaspImportsText = T.pack $ intercalate "\n" fixedWaspImports
    nonWaspImportsText = T.pack $ intercalate "\n" nonWaspImports
    remainingCodeText = T.pack $ intercalate "\n" remainingCode
    fixedWaspImports = map (importsForNames M.!) importedNames
    importedNames = nub $ concatMap getImportedNames waspImports
    (waspImports, nonWaspImports, remainingCode) = partitionComponentFile pageComponentContent

getImportedNames :: String -> [String]
getImportedNames importLine = nub $ words $ map removeSpecialCharacters middleOfImportLine
  where
    middleOfImportLine =
      trim $
        removeSuffix "from" $
          trim $
            removePrefix "import " $
              trim importLineWithoutPath
    importLineWithoutPath = takeWhile (not . (`elem` ['"', '\''])) importLine
    removeSpecialCharacters char
      | char `elem` [',', '}', '{'] = ' '
      | otherwise = char
    removePrefix prefix = fromJust . stripPrefix prefix
    removeSuffix suffix = reverse . removePrefix (reverse suffix) . reverse

partitionComponentFile :: Text -> ([String], [String], [String])
partitionComponentFile componentContent = (waspImportLines, nonWaspImportLines, remainingCodeLines)
  where
    (waspImportLines, nonWaspImportLines) = partition ("@wasp" `isInfixOf`) importLines
    (importLines, remainingCodeLines) = splitContentIntoImportsAndRest $ T.unpack $ T.strip componentContent
    splitContentIntoImportsAndRest = span (isPrefixOf "import" . trim) . lines

getWaspImportsForNames :: [Operation] -> M.Map String String
getWaspImportsForNames operations = M.fromList $ possibleUnchangingImports ++ map makeOperationImport operations
  where
    possibleUnchangingImports :: [(String, String)]
    possibleUnchangingImports =
      [ ("logout", "import logout from '@wasp/auth/logout';"),
        ("useAuth", "import useAuth from '@wasp/auth/useAuth';"),
        ("useQuery", "import { useQuery } from '@wasp/queries';"),
        ("useAction", "import { useAction } from '@wasp/actions';")
      ]

    makeOperationImport :: Operation -> (String, String)
    makeOperationImport operation = (T.unpack opName, T.unpack opImport)
      where
        opImport = [trimming|import ${opName} from '@wasp/${opType}/${opName}';|]
        opName = T.pack $ Plan.opName $ opPlan operation
        opType = case Operation.opType operation of
          Operation.Action -> "actions"
          Operation.Query -> "queries"

fixImportsOld :: FilePath -> [Operation] -> [Operation] -> CodeAgent ()
fixImportsOld pageComponentPath queries actions = do
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  trace ("input:" ++ T.unpack currentPageComponentContent) $ return ()
  let x@(waspImportsText, nonWaspImportsText, remainingCodeText) = partitionComponentFileText currentPageComponentContent
  trace ("output:" ++ show x) $ return ()
  fixedWaspImportsText <-
    queryChatGPTForJSON
      (defaultChatGPTParamsForFixing {_model = importFixGptModel})
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = importsPrompt waspImportsText}
      ]
  let fixedComponentContent = T.intercalate "\n" [nonWaspImportsText, imports fixedWaspImportsText, remainingCodeText]
  writeToFile pageComponentPath (const fixedComponentContent)
  where
    importsPrompt waspImportsText =
      [trimming|
      We want to write a list of JavaScript imports. These are the imports we are allowed to choose from:
        ${possibleWaspImports}

      A junior developer wrote these imports:

      ```js
      ${waspImportsText}
      ```

      Let's fix the imports by choosing the ones we need from the initial list of valid imports.
      Only include imports for the symbols the junior developer wanted to use, don't add any redundant imports.

      Please respond ONLY with a valid JSON of the format { imports: string }.
      There should be no other text in your response. Don't wrap content with the "```" code delimiters.
      |]
    possibleWaspImports = T.intercalate "\n" $ map ("- " <>) $ getAllPossibleImports $ queries ++ actions
    importFixGptModel = GPT_4

partitionComponentFileText :: Text -> (Text, Text, Text)
partitionComponentFileText componentContent = (waspImportsText, nonWaspImportsText, remainingCodeText)
  where
    waspImportsText = T.pack $ intercalate "\n" waspImportLines
    nonWaspImportsText = T.pack $ intercalate "\n" nonWaspImportLines
    remainingCodeText = T.pack $ intercalate "\n" remainingCodeLines
    (waspImportLines, nonWaspImportLines) = partition ("@wasp" `isInfixOf`) importLines
    (importLines, remainingCodeLines) = splitContentIntoImportsAndRest $ T.unpack $ T.strip componentContent
    splitContentIntoImportsAndRest = span (isPrefixOf "import" . trim) . lines

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
            - Ensure query and action imports are correct. One import per query / action, default imports,
              name of the file same as name of the query.
            - Use Tailwind CSS to style the page if you didn't.
            - Use <Link /> component from "react-router-dom" to link to other pages where needed.
            - "TODO" comments or "..." that should be replaced with actual implementation.
              Fix these by replacing them with actual implementation.
            - If there are any duplicate imports, make sure to remove them.
            - There might be some invalid JS or JSX syntax -> fix it if there is any.
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

fixImportsInPageComponent :: FilePath -> [Operation] -> [Operation] -> CodeAgent ()
fixImportsInPageComponent pageComponentPath queries actions = do
  currentPageComponentContent <- fromMaybe (error "couldn't find page file to fix") <$> getFile pageComponentPath
  fixedPageComponent <-
    queryChatGPTForJSON
      defaultChatGPTParamsForFixing
      [ ChatMessage {role = System, content = Prompts.systemPrompt},
        ChatMessage {role = User, content = fixImportsInPageComponentPrompt currentPageComponentContent}
      ]
  writeToFile pageComponentPath (const $ pageComponentImpl fixedPageComponent)
  where
    fixImportsInPageComponentPrompt currentPageContent =
      [trimming|
          Here is a React component (${pageComponentPathText}) containing some frontend code
          that we generated together earlier:
          ```js
          ${currentPageContent}
          ```

          Some imports are probably incorrect. Let's fix them. 

          Make sure that every Wasp imports (i.e., imports from '@wasp') is one of the following: 
          ${possibleImportsList}
          This is an exhaustive list of all valid Wasp imports.
          Any Wasp import not found in the list is invalid, fix it by replacing it with one or more appropriate imports from the list above.
      |]
    pageComponentPathText = T.pack pageComponentPath
    possibleImportsList = T.unlines $ getAllPossibleImports $ queries ++ actions

data Imports = Imports
  { imports :: Text
  }
  deriving (Generic, Show)

instance FromJSON Imports

data PageComponent = PageComponent
  { pageComponentImpl :: Text
  }
  deriving (Generic, Show)

instance FromJSON PageComponent
