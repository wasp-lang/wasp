module Wasp.Cli.Command.AI.GenerateNewProject
  ( generateNewProject,
    NewProjectDetails (..),
    AuthProvider (..),
  )
where

-- TODO: Probably move this module out of here into general wasp lib.

import Control.Arrow (first)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent, writeNewFile, writeToLog)
import Wasp.Cli.Command.AI.GenerateNewProject.Plan (Plan)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as P
import Wasp.Cli.Command.CreateNewProject (readCoreWaspProjectFiles)
import qualified Wasp.Version

data NewProjectDetails = NewProjectDetails
  { _projectAppName :: !String,
    _projectDescription :: !String,
    _projectAuth :: !AuthProvider
  }

-- TODO: Make these relative to WaspProjectDir?
type File = (FilePath, Text)

-- TODO: Support more methods.
data AuthProvider = UsernameAndPassword

-- TODO: Have generateNewProject accept Chan, to which it will stream its progress?
--   It could just stream its output instead of printing it to stdout, so calling function
--   has more control over what to do with it.
--   Yeah, I think we certainly want to have Chan. And one important thing we want to send over it
--   is information about files that we are updating. So each time we create a new file or update existing one,
--   we want to send over information about that, so receiver can either write it to disk, or show it in web app,
--   or something. Such messages could be called "FileWritten" and say which path, what is the new content,
--   and also contain description of what happened (or maybe that is separate message).
generateNewProject :: NewProjectDetails -> CodeAgent ()
generateNewProject newProjectDetails = do
  waspFilePath <- generateAndWriteProjectSkeleton

  writeToLog "Generating plan..."
  plan <- generatePlan newProjectDetails
  -- TODO: Show plan nicer! Maybe just short summary of it: we will create 4 entities, 3 operations, ... .
  writeToLog $ "Plan generated! " <> T.pack (show plan)

  writeEntitiesToWaspFile waspFilePath (P.entities plan)
  writeToLog "Added entities to wasp file."

  writeToLog "Generating actions..."
  actions <- forM (P.actions plan) $ generateAndWriteAction waspFilePath plan

  writeToLog "Generating queries..."
  queries <- forM (P.queries plan) $ generateAndWriteQuery waspFilePath plan

  writeToLog "Generating pages..."
  _pages <- forM (P.pages plan) $ generateAndWritePage waspFilePath plan queries actions

  -- TODO: what about having additional step here that goes through all the files once again and fixes any stuff in them (Wasp, JS files)? REPL?
  -- TODO: add some commented out lines to wasp file that showcase other features? jobs, api, serverSetup, sockets, ... .
  -- TODO: Idea: give it chatGPT-function `queryDocs` that it can use whenever to look up Wasp docs.
  -- TODO: Idea: maybe use chatGPT-functions also to increase the chance of it producing correct JSON
  --   when generating actions, operations and similar. Maybe an overkill.
  writeToLog "Done!"
  where
    generateAndWriteProjectSkeleton = do
      coreFiles <- liftIO $ map (first SP.fromRelFile) <$> readCoreWaspProjectFiles
      mapM_ writeNewFile coreFiles

      let waspFile@(waspFilePath, _) = generateBaseWaspFile newProjectDetails
      writeNewFile waspFile

      case _projectAuth newProjectDetails of
        UsernameAndPassword -> do
          writeNewFile generateLoginJsPage
          writeNewFile generateSignupJsPage

      writeNewFile generateDotEnvServerFile

      writeToLog "Generated project skeleton."

      return waspFilePath

    generateAndWriteAction waspFilePath plan actionPlan = do
      action <- generateAction newProjectDetails (P.entities plan) actionPlan
      writeActionToFile action
      writeActionToWaspFile waspFilePath action
      writeToLog $ "Generated action: " <> T.pack (P.actionName actionPlan)
      return action

    generateAndWriteQuery waspFilePath plan queryPlan = do
      query <- generateQuery newProjectDetails (P.entities plan) queryPlan
      writeQueryToFile query
      writeQueryToWaspFile waspFilePath query
      writeToLog $ "Generated query: " <> T.pack (P.queryName queryPlan)
      return query

    generateAndWritePage waspFilePath plan queries actions pagePlan = do
      page <- generatePage newProjectDetails (P.entities plan) queries actions pagePlan
      writePageToFile page
      writePageToWaspFile waspFilePath page
      writeToLog $ "Generated page: " <> T.pack (P.pageName pagePlan)
      return page

generateBaseWaspFile :: NewProjectDetails -> File
generateBaseWaspFile newProjectDetails = (path, content)
  where
    path = "main.wasp"
    appName = T.pack $ _projectAppName newProjectDetails
    appTitle = appName
    waspVersion = T.pack $ show Wasp.Version.waspVersion
    appAuth = case _projectAuth newProjectDetails of
      -- NOTE: We assume here that there will be a page with route "/".
      UsernameAndPassword ->
        [trimming|
          auth: {
            userEntity: User,
            methods: {
              usernameAndPassword: {}
            },
            onAuthFailedRedirectTo: "/login",
            onAuthSucceededRedirectTo: "/"
          },
        |]
    content =
      [trimming|
        app ${appName} {
          wasp: {
            version: "^${waspVersion}"
          },
          title: ${appTitle},
          ${appAuth}
        }

        entity User {=psl
          id                        Int           @id @default(autoincrement())
          username                  String        @unique
          password                  String
        psl=}

        route LoginRoute { path: "/login", to: LoginPage }
        page LoginPage {
          component: import Login from "@client/Login.jsx"
        }
        route SignupRoute { path: "/signup", to: SignupPage }
        page SignupPage {
          component: import Signup from "@client/Signup.jsx"
        }
      |]

generateLoginJsPage :: File
generateLoginJsPage =
  ( "src/client/Login.jsx",
    [trimming|
      import React from 'react';
      import { LoginForm } from '@wasp/auth/forms/Login';

      export default function Login() {
        return (
          <div>
            <h1>Login</h1>
            <LoginForm />
          </div>
        );
      }
    |]
  )

generateSignupJsPage :: File
generateSignupJsPage =
  ( "src/client/Signup.jsx",
    [trimming|
      import React from 'react';
      import { SignupForm } from '@wasp/auth/forms/Signup';

      export default function Signup() {
        return (
          <div>
            <h1>Signup</h1>
            <SignupForm />
          </div>
        );
      }
    |]
  )

generateDotEnvServerFile :: File
generateDotEnvServerFile =
  ( ".env.server",
    [trimming|
      // Here you can define env vars to pass to the server.
      // MY_ENV_VAR=foobar
    |]
  )

generatePlan :: NewProjectDetails -> CodeAgent Plan
generatePlan = undefined

--       [ ChatMessage
--           { role = System,
--             content = "You are an expert Wasp developer, helping set up a new Wasp project."
--           },
--         ChatMessage
--           { role = User,
--             -- TODO: I should tell it to mark the type of each ext import: "page", "query", "action".
--             content = "Hi"
--           }
--       ]

-- TODO: Tell it to generate at least one page which has route "/".

writeEntitiesToWaspFile :: FilePath -> [P.Entity] -> CodeAgent ()
writeEntitiesToWaspFile waspFilePath entities = do
  -- TODO: assemble code for each entity and write it to wasp file.
  undefined

generateAction :: NewProjectDetails -> [P.Entity] -> P.Action -> CodeAgent Action
generateAction = undefined

writeActionToFile :: Action -> CodeAgent ()
writeActionToFile = undefined

writeActionToWaspFile :: FilePath -> Action -> CodeAgent ()
writeActionToWaspFile waspFilePath action = undefined

data Action = Action
  { _actionWaspDecl :: String,
    _actionJsImpl :: String,
    _actionPlan :: P.Action
  }

generateQuery :: NewProjectDetails -> [P.Entity] -> P.Query -> CodeAgent Query
generateQuery = undefined

writeQueryToFile :: Query -> CodeAgent ()
writeQueryToFile = undefined

writeQueryToWaspFile :: FilePath -> Query -> CodeAgent ()
writeQueryToWaspFile waspFilePath query = undefined

data Query = Query
  { _queryWaspDecl :: String,
    _queryJsImpl :: String,
    _queryPlan :: P.Action
  }

generatePage :: NewProjectDetails -> [P.Entity] -> [Query] -> [Action] -> P.Page -> CodeAgent Page
generatePage = undefined

writePageToFile :: Page -> CodeAgent ()
writePageToFile = undefined

writePageToWaspFile :: FilePath -> Page -> CodeAgent ()
writePageToWaspFile waspFilePath page = undefined

data Page = Page
  { _pageWaspDecl :: String,
    _pageJsImpl :: String,
    _pagePlan :: P.Action
  }

waspFileExample =
  [trimming|
  Example main.wasp (comments are explanation for you):

  ```wasp
    app todoApp {
      wasp: { version: "^0.10.2" },
      title: "ToDo App",
      auth: {
        userEntity: User,
        // Define only if using social (google) auth.
        externalAuthEntity: SocialLogin,
        methods: {
          usernameAndPassword: {},
          google: {
            configFn: import { config } from "@server/auth/google.js",
            getUserFieldsFn: import { getUserFields } from "@server/auth/google.js"
          },
        },
        onAuthFailedRedirectTo: "/login",
        onAuthSucceededRedirectTo: "/"
      }
    }

    // psl stands for Prisma Schema Language.
    entity User {=psl
        id                        Int           @id @default(autoincrement())
        username                  String        @unique
        password                  String
        tasks                     Task[]
        externalAuthAssociations  SocialLogin[] // Only if using social auth.
    psl=}

    // Define only if using social auth (e.g. google).
    entity SocialLogin {=psl
      id          Int       @id @default(autoincrement())
      provider    String
      providerId  String
      user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
      userId      Int
      createdAt   DateTime  @default(now())
      @@unique([provider, providerId, userId])
    psl=}

    // Ommiting entity Task to keep the example short.

    route SignupRoute { path: "/signup", to: SignupPage }
    page SignupPage {
      component: import Signup from "@client/pages/auth/Signup.jsx" // REQ.
    }

    // Ommiting LoginRoute and LoginPage to keep the example short.

    route HomeRoute { path: "/", to: MainPage }
    page MainPage {
      authRequired: true,
      component: import Main from "@client/pages/Main.jsx"
    }

    // Queries are nodejs functions that do R in CRUD.
    query getTasks {
      fn: import { getTasks } from "@server/queries.js", // REQ
      entities: [Task]
    }

    // Actions are like quries but do CUD in CRUD.
    action createTask {
      fn: import { createTask } from "@server/actions.js",
      entities: [Task]
    }
  ```
  |]

-- TODO: Explain to ChatGPT how data flows in Wasp: pages call actions and queries,
--   actions and queries work with entities, entities are the core data models.
--   Also maybe provide a bit more details on what each of these do.
basicWaspLangInfo =
  [trimming|
  Wasp is web app framework that uses React, NodeJS and Prisma.
  High-level is described in main.wasp file, details in JS/JSX files.
  Main Wasp features: frontend Routes and Pages, Queries and Actions (RPC), Entities.
  |]
