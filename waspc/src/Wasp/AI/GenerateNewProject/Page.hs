{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.GenerateNewProject.Page
  ( generateAndWritePage,
    makePageDocPrompt,
    getPageComponentPath,
    operationInfo,
    Page (..),
  )
where

import Data.Aeson (FromJSON)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Wasp.AI.CodeAgent (CodeAgent, writeToFile, writeToLog)
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    defaultChatGPTParams,
    queryChatGPTForJSON,
    writeToWaspFileEnd,
  )
import qualified Wasp.AI.GenerateNewProject.Common.Prompts as Prompts
import Wasp.AI.GenerateNewProject.Entity (entityPlanToWaspDecl)
import Wasp.AI.GenerateNewProject.Operation (Operation (opImpl, opPlan), OperationImpl (opJsImpl))
import qualified Wasp.AI.GenerateNewProject.Plan as Plan
import Wasp.AI.OpenAI.ChatGPT (ChatMessage (..), ChatRole (..))

generateAndWritePage ::
  NewProjectDetails -> FilePath -> [Plan.Entity] -> [Operation] -> [Operation] -> Plan.Page -> CodeAgent Page
generateAndWritePage newProjectDetails waspFilePath entityPlans queries actions pPlan = do
  page <- generatePage newProjectDetails entityPlans queries actions pPlan
  writePageToJsFile page
  writePageToWaspFile waspFilePath page
  writeToLog $ T.pack $ "Generated page: " <> Plan.pageName pPlan
  return page

generatePage :: NewProjectDetails -> [Plan.Entity] -> [Operation] -> [Operation] -> Plan.Page -> CodeAgent Page
generatePage newProjectDetails entityPlans queries actions pPlan = do
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
    routeName = T.pack $ Plan.routeName pPlan
    routePath = T.pack $ Plan.routePath pPlan
    pageDesc = T.pack $ Plan.pageDesc pPlan

    entityDecls = T.intercalate "\n\n" $ entityPlanToWaspDecl <$> entityPlans
    queriesInfo = T.intercalate "\n" $ (" - " <>) . operationInfo <$> queries
    actionsInfo = T.intercalate "\n" $ (" - " <>) . operationInfo <$> actions
    pageDocPrompt = makePageDocPrompt

    planPrompt =
      [trimming|
        ${basicWaspLangInfoPrompt}

        ===============

        ${pageDocPrompt}

        ===============

        We are implementing a Wasp app (check bottom for description).

        Entities in our app:
        ${entityDecls}

        Actions in our app:
        ${actionsInfo}

        Queries in our app:
        ${queriesInfo}

        We use Tailwind to style pages. Example of Tailwind usage:

        ```jsx
          <div className="p-4 bg-slate-50 rounded-lg">
            <button className="bg-slate-500 hover:bg-slate-700 text-white font-bold py-2 px-4 rounded">
              Button
            </button>
          </div>
        ```

        ===============

        Let's now implement the following Wasp page:
         - name: ${pageName}
         - component: ${componentPath}
         - routeName: ${routeName}
         - routePath: ${routePath}
         - description: ${pageDesc}

        Please, respond ONLY with a valid JSON, of following format:
        { "pageWaspDecl": string, "pageJsImpl": string }

        Example of such JSON:
        {
          "pageWaspDecl": "route ExampleRoute { path: \"/\", to: ExamplePage }\npage ExamplePage {\n  component: import { ExamplePage } from \"@client/ExamplePage.jsx\",\n  authRequired: true\n}",
          "pageJsImpl": "JS imports + React component implementing the page.",
        }
        There should be no other text in the response.

        ${appDescriptionBlock}
      |]

makePageDocPrompt :: Text
makePageDocPrompt =
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

        Example of ReactJS implementation (you can use Tailwind CSS for styling):

        ```jsx
        import React, { useState } from 'react';
        import logout from '@wasp/auth/logout';
        import { Link } from 'react-router-dom';
        import useAuth from '@wasp/auth/useAuth';
        import { useQuery } from '@wasp/queries'; // A thin wrapper around react-query's useQuery
        import { useAction } from '@wasp/actions'; // A thin wrapper around react-query's useMutation
        import getTasks from '@wasp/queries/getTasks';
        import createTask from '@wasp/actions/createTask';
        import toggleTask from '@wasp/actions/toggleTask';
        import editTask from '@wasp/actions/editTask';

        export function DashboardPage() {
          const { data: user } = useAuth();
          const { data: tasks, isLoading, error } = useQuery(getTasks);
          const createTaskFn = useAction(createTask);
          const toggleTaskFn = useAction(toggleTask);
          const editTaskFn = useAction(editTask);
          const [newTaskDescription, setNewTaskDescription] = useState('');

          if (isLoading) return 'Loading...';
          if (error) return 'Error: ' + error;

          const handleCreateTask = (event) => {
            event.preventDefault();
            createTaskFn({ description: newTaskDescription });
            setNewTaskDescription('');
          };

          return (
            <div className='w-full h-full'>
              <div className='min-w-full min-h-[75vh] flex flex-col items-center'>
                <h1 className='text-5xl font-bold mt-20 mb-5'>
                  {user.username}'s todo list
                </h1>
                <div className='w-full h-full max-w-lg p-5'>
                  <form className='container flex gap-x-4 py-5'>
                    <input
                      type='text'
                      placeholder='New Task'
                      className='flex-grow px-1 py-2 border border-gray-300 rounded text-lg'
                      value={newTaskDescription}
                      onChange={(e) => setNewTaskDescription(e.target.value)}
                    />
                    <button
                      onClick={handleCreateTask}
                      className='bg-blue-500 hover:bg-blue-700 px-2 py-2 text-white font-bold rounded'
                    >
                      Add Task
                    </button>
                  </form>
                  <div className='container rounded'>
                    {tasks.map((task) => (
                      <div
                        key={task.id}
                        className='py-2 px-2 container flex items-center hover:bg-slate-100 gap-x-2 rounded'
                      >
                        <input
                          type='text'
                          value={task.description}
                          onChange={(e) =>
                            editTaskFn({
                              id: task.id,
                              description: e.target.value,
                            })
                          }
                          className='bg-inherit focus:bg-white px-1 py-1 rounded outline-0 border border-transparent focus:border-black flex-grow'
                        />

                        <input
                          type='checkbox'
                          checked={task.isDone}
                          onChange={() => toggleTaskFn({ id: task.id })}
                          className='mr-2 h-6 w-6'
                        />
                      </div>
                    ))}
                  </div>
                </div>
              </div>
            </div>
          );
        }
        ```

        Here are the rules for importing actions and queries.

        If a query is called "myQuery", its import MUST BE `import myQuery from '@wasp/queries/myQuery';`.
        More generally, a query import MUST BE a default import, and name of the file is the same as name of the query.
        The hook for wrapping queries is called `useQuery`.
        Use a single import statement per query.

        If an action is called "myAction", its import MUST BE `import myAction from '@wasp/actions/myAction';`.
        More generally, an action import MUST BE a default import, and name of the file is the same as name of the action.
        The hook for wrapping actions is called `useAction`.
        Use a single import statement per action.

        Note: There is no `useMutation` hook in Wasp.
      |]

operationInfo :: Operation -> Text
operationInfo operation =
  -- TODO: Potential optimization would be to show operation args and what it returns, not the whole
  -- implementation. However for short operations, it is just easier to show whole implementation.
  [trimming|
    { "name": ${name},
      "jsImpl": ${jsImpl}
    }
  |]
  where
    name = T.pack $ show $ Plan.opName $ opPlan operation
    jsImpl = T.pack $ show $ opJsImpl $ opImpl operation

writePageToJsFile :: Page -> CodeAgent ()
writePageToJsFile page =
  writeToFile path $ (<> "\n\n" <> jsImpl) . fromMaybe ""
  where
    path = getPageComponentPath page
    jsImpl = T.pack $ pageJsImpl $ pageImpl page

getPageComponentPath :: Page -> String
getPageComponentPath page = path
  where
    path = resolvePath $ Plan.componentPath $ pagePlan page
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
