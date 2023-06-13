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
  coreFiles <- liftIO $ map (first SP.fromRelFile) <$> readCoreWaspProjectFiles
  mapM_ writeNewFile coreFiles
  let waspFile = generateBaseWaspFile newProjectDetails
  let waspFilePath = fst waspFile
  writeNewFile waspFile
  let dotEnvServerFile = generateDotEnvServerFile newProjectDetails
  writeNewFile dotEnvServerFile
  writeToLog "Generated project skeleton."

  writeToLog "Generating plan..."
  plan <- generatePlan newProjectDetails
  -- TODO: Show plan nicer! Maybe just short summary of it: we will create 4 entities, 3 operations, ... .
  writeToLog $ "Plan generated! " <> T.pack (show plan)

  updateWaspFileWithEntities waspFilePath (P.entities plan)
  writeToLog "Added entities to wasp file."

  writeToLog "Generating actions..."
  actions <- forM (P.actions plan) $ \actionPlan -> do
    action <- generateAction newProjectDetails (P.entities plan) actionPlan
    writeActionToFile action
    updateWaspFileWithAction waspFilePath action
    writeToLog $ "Generated action: " <> T.pack (P.actionName actionPlan)
    return action

  writeToLog "Generating queries..."
  queries <- forM (P.queries plan) $ \queryPlan -> do
    query <- generateQuery newProjectDetails (P.entities plan) queryPlan
    writeQueryToFile query
    updateWaspFileWithQuery waspFilePath query
    writeToLog $ "Generated query: " <> T.pack (P.queryName queryPlan)
    return query

  writeToLog "Generating pages..."
  pages <- forM (P.pages plan) $ \pagePlan -> do
    page <- generatePage newProjectDetails (P.entities plan) queries actions pagePlan
    writePageToFile page
    updateWaspFileWithPage waspFilePath page
    writeToLog $ "Generated page: " <> T.pack (P.pageName pagePlan)
    return page

  -- TODO: what about having additional step here that goes through all the files once again and fixes any stuff in them (Wasp, JS files)? REPL?
  -- TODO: add some commented out lines to wasp file that showcase other features? jobs, api, serverSetup, sockets, ... .
  writeToLog "Done!"

-- TODO: OpenAI released ChatGPT 3.5-turbo with 16k context, should we use that one?
--   What about "functions" feature that they released?

generateBaseWaspFile :: NewProjectDetails -> File
generateBaseWaspFile = undefined

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

generateDotEnvServerFile :: NewProjectDetails -> File
generateDotEnvServerFile = undefined

generatePlan :: NewProjectDetails -> CodeAgent Plan
generatePlan = undefined

updateWaspFileWithEntities :: FilePath -> [P.Entity] -> CodeAgent ()
updateWaspFileWithEntities waspFilePath entities = do
  -- TODO: assemble code for each entity and write it to wasp file.
  undefined

generateAction :: NewProjectDetails -> [P.Entity] -> P.Action -> CodeAgent Action
generateAction = undefined

writeActionToFile :: Action -> CodeAgent ()
writeActionToFile = undefined

updateWaspFileWithAction :: FilePath -> Action -> CodeAgent ()
updateWaspFileWithAction waspFilePath action = undefined

data Action = Action
  { _actionWaspDecl :: String,
    _actionJsImpl :: String,
    _actionPlan :: P.Action
  }

generateQuery :: NewProjectDetails -> [P.Entity] -> P.Query -> CodeAgent Query
generateQuery = undefined

writeQueryToFile :: Query -> CodeAgent ()
writeQueryToFile = undefined

updateWaspFileWithQuery :: FilePath -> Query -> CodeAgent ()
updateWaspFileWithQuery waspFilePath query = undefined

data Query = Query
  { _queryWaspDecl :: String,
    _queryJsImpl :: String,
    _queryPlan :: P.Action
  }

generatePage :: NewProjectDetails -> [P.Entity] -> [Query] -> [Action] -> P.Page -> CodeAgent Page
generatePage = undefined

writePageToFile :: Page -> CodeAgent ()
writePageToFile = undefined

updateWaspFileWithPage :: FilePath -> Page -> CodeAgent ()
updateWaspFileWithPage waspFilePath page = undefined

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

basicWaspLangInfo =
  [trimming|
  Wasp is web app framework that uses React, NodeJS and Prisma.
  High-level is described in main.wasp file, details in JS/JSX files.
  Main Wasp features: frontend Routes and Pages, Queries and Actions (RPC), Entities.
  |]
