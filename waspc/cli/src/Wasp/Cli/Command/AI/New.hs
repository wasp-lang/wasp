{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Wasp.Cli.Command.AI.New
  ( new,
    queryChatGpt,
    sayHiToChatGpt,
  )
where

import Control.Arrow ()
import Control.Monad.Except (MonadIO (liftIO))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 as BSU
import Data.Text (Text)
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import qualified Network.HTTP.Simple as HTTP
import StrongPath (Abs, Dir, Path', fromAbsDir, fromRelFile, relfile, (</>))
import StrongPath.Operations ()
import System.Directory (getFileSize, setCurrentDirectory)
import qualified System.Environment as System.Environment
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, readWaspCompileInfo)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import qualified Wasp.Cli.Command.CreateNewProject as CNP
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Start.Db (getDbSystem)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import Wasp.Util.IO (removeFile)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

new :: Command ()
new = do
  (webAppTitle, webAppDescription) <- liftIO $ do
    putStrLn "Describe the web app you want to create:"
    putStrLn "Title:"
    title <- getLine
    putStrLn "What it should work like:"
    desc <- getLine
    return (title, desc)

  absWaspProjectDir <- createNewEmptyProject webAppTitle
  liftIO $ setCurrentDirectory $ fromAbsDir absWaspProjectDir

  waspFileContent <- aiWriteWaspFile absWaspProjectDir webAppTitle webAppDescription

  aiWriteWaspPages absWaspProjectDir waspFileContent
  aiWriteWaspOperations absWaspProjectDir waspFileContent
  -- Maybe write something else also: setupFn or something.

  return ()
  where
    -- appSpec <- analyze waspDir
    -- let (appName, app) = ASV.getApp appSpec
    testAppDesc =
      [trimming|
      Simple app that enables inputing pokemons and then on request can produce a random fight between the two of them.
      It should have authentication, so each user has their own pokemon, but fights happen with random pokemon
      of another user.
      |]

    createNewEmptyProject :: String -> Command (Path' Abs (Dir WaspProjectDir))
    createNewEmptyProject webAppTitle = do
      projectInfo <- CNP.parseProjectInfo webAppTitle
      CNP.createWaspProjectDir projectInfo
      absWaspProjectDir <- CNP.getAbsoluteWaspProjectDir projectInfo
      -- Delete existing source files that we generate in the new project.
      -- TODO: Instead of deleting files, I should instead have a function that generates
      --   the very basic skeleton for the Wasp app, and then the normal "new app" would
      --   just add files to it.
      liftIO $ do
        removeFile $ absWaspProjectDir </> [relfile|main.wasp|]
        removeFile $ absWaspProjectDir </> [relfile|src/client/Main.css|]
        removeFile $ absWaspProjectDir </> [relfile|src/client/MainPage.jsx|]
        removeFile $ absWaspProjectDir </> [relfile|src/client/waspLogo.png|]
      return absWaspProjectDir

    -- Writes wasp file to disk, but also returns its content.
    -- TODO: Also check if it compiles and if not, send errors to GPT.
    aiWriteWaspFile :: Path' Abs (Dir WaspProjectDir) -> String -> Text -> Command Text
    aiWriteWaspFile absWaspProjectDir appTitle appDesc = do
      -- TODO: Tell GPT about Wasp in general, shortly.
      -- Also give it an example of a Wasp file that is pretty rich. It can even be done as a
      -- previous part of the conversation, so it has an example of what is good.
      -- Then, tell it to generate a Wasp file for the given prompt, while also adding comments in
      -- it for every ExtImport, where comments explain what that file is supposed to contain / do,
      -- so basically to serve as instrutioncs to itself.
      -- Or probably best to tell it to provide those instructions separately, in a JSON object
      -- where key is the name of each page or whatever.
      -- Once it does, let's feed that Wasp file to the Wasp analyzer and see if it returns any
      -- errors. If it does, send it back to chat GPT for repairs.
      -- Finally, write it to disk.
      -- In the example wasp file, we can put in a lot of comments explaining stuff, but then we can
      -- ask it to not produce those once it produces a wasp file, so we save some tokens.
      -- We should also make it clear which feature in wasp file is related to which part of the
      -- prompt, so it can know to skip them properly.
      -- TODO: What if it fails to repair it? Well we can just pretend all is ok, let user fix it.
      -- TODO: Ask chatGPT to compress our prompt for us.
      let chatMessages =
            [ ChatMessage
                { role = System,
                  content = "You are an expert Wasp developer, helping set up a new Wasp project."
                },
              ChatMessage
                { role = User,
                  -- TODO: I should tell it to mark the type of each ext import: "page", "query", "action".
                  content =
                    [trimming|
  Wasp is web app framework that uses React, NodeJS and Prisma.
  High-level is described in main.wasp file, details in JS/JSX files.
  Main Wasp features: frontend Routes and Pages, Queries and Actions (RPC), Entities.

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

    // TODO: implement entity Task.

    route SignupRoute { path: "/signup", to: SignupPage }
    page SignupPage {
      component: import Signup from "@client/pages/auth/Signup.jsx" // REQ.
    }

    // TODO: implement LoginPage, analogous to SignupPage.

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

  Now, I will describe a new Wasp app to you.

  You will first respond with only the content of main.wasp file (no comments).

  Then, you will print a line that goes like this: "---------------------------", to mark the ending of the wasp file content. This is very important, make sure to do this.

  Finally, for every external import, provide instructions on how to implement the corresponding JS function/component.
  The instructions should be in a JSON format like this:
  [{ import: "import { createTask } from \"@server/tasks.js\"", in: "page", instruction: "..." }, ...].
  `in` field can be "page", "query" or "action".

  Everything after this sentence is app description:

  ${appDesc}
  |]
                }
            ]
      error "TODO"

    aiWriteWaspPages :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspPages absWaspProjectDir waspFileContent = do
      -- TODO: Actually it should recieve AppSpec I think, right?
      -- So idea is here that for each page we give Wasp file to Chat GPT, also all the basic
      -- knowledge about Wasp, and then ask it to generate JS for that page. If page already exists,
      -- we also pass that and tell it to add stuff to it.
      --
      -- How do we get comments to it about it? Well, maybe it is smart enough to pick them up from
      -- the wasp file?
      -- Or, we give them separately, but then we need them in a good format. I think it will be
      -- able to pick them up on its own.
      -- Oh and we also need to give it info about the concept of the page itself! And example. Uff.
      -- Maybe that is too much and we can't give it all that. In that case we should drop the idea
      -- of passing whole Wasp file, and we need to give it only instructions for that page. We can
      -- have it write those instructions separately, for each page, in that case. Yeah probably
      -- that is the best.
      -- Hm and we also need initial prompt by user here.
      error "TODO"

    aiWriteWaspOperations :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspOperations absWaspProjectDir waspFileContent = do
      -- Here we do everything analogous as we did for the pages, but it becomes extra important to be able to reuse the already written file, so we should make sure we have that going,
      -- because often it is normal to put multiple operations in the same file.
      error "TODO"

-- TODO: What about other ext imports? Make sure we cover all of them: jobs, setupFn (server, client), api, something else?

-- In general, gpt-3.5-turbo-0301 does not pay strong attention to the system message, and therefore important instructions are often better placed in a user message.

sayHiToChatGpt :: IO ()
sayHiToChatGpt = do
  apiKey <- System.Environment.getEnv "OPENAI_API_KEY"
  answer <- queryChatGpt apiKey [ChatMessage {role = User, content = "What is 2 + 2?"}]
  print answer

-- TODO: We will need to have this on a server somewhere, not here, if we want to use our API keys.
--   If we let them use their API keys then it can be here.
queryChatGpt :: String -> [ChatMessage] -> IO Text
queryChatGpt apiKey requestMessages = do
  -- TODO: We could try playing with parameters like temperature, max_tokens, ... .
  --   I noticed it works quite well with temperature of 1!
  let reqBodyJson =
        Aeson.object
          [ "model" .= ("gpt-3.5-turbo" :: String),
            "messages" .= requestMessages
          ]
      request =
        HTTP.setRequestHeader "Authorization" [BSU.fromString $ "Bearer " <> apiKey] $
          HTTP.setRequestBodyJSON reqBodyJson $
            HTTP.parseRequest_ "POST https://api.openai.com/v1/chat/completions"

  -- TODO: Consider using httpJSONEither here, so I can handle errors better.
  response <- HTTP.httpJSON request

  -- TODO: I should probably check status code here, confirm it is 200.
  let responseStatusCode = HTTP.getResponseStatusCode response
      (chatResponse :: ChatResponse) = HTTP.getResponseBody response
  putStrLn $ "Response status code:" <> show responseStatusCode
  putStrLn $ "Response body:" <> show chatResponse

  return $ content $ message $ head $ choices chatResponse

data ChatResponse = ChatResponse
  { id :: !String,
    object :: !String,
    created :: !Int,
    model :: !String,
    choices :: ![ChatResponseChoice],
    usage :: !ChatResponseUsage
  }
  deriving (Generic, Show)

instance Aeson.FromJSON ChatResponse

data ChatResponseUsage = ChatResponseUsage
  { prompt_tokens :: !Int,
    completion_tokens :: !Int,
    total_tokens :: !Int
  }
  deriving (Generic, Show)

instance Aeson.FromJSON ChatResponseUsage

data ChatResponseChoice = ChatResponseChoice
  { index :: !Int,
    message :: !ChatMessage,
    finish_reason :: !String
  }
  deriving (Generic, Show)

instance Aeson.FromJSON ChatResponseChoice

data ChatMessage = ChatMessage
  { role :: !ChatRole,
    content :: !Text
  }
  deriving (Generic, Show)

instance Aeson.ToJSON ChatMessage

instance Aeson.FromJSON ChatMessage

data ChatRole = User | System | Assistant
  deriving (Generic, Show)

instance Aeson.ToJSON ChatRole where
  toJSON User = "user"
  toJSON System = "system"
  toJSON Assistant = "assistant"

instance Aeson.FromJSON ChatRole where
  parseJSON = Aeson.withText "ChatRole" $ \case
    "user" -> return User
    "system" -> return System
    "assistant" -> return Assistant
    other -> fail $ "Invalid ChatRole: " <> show other
