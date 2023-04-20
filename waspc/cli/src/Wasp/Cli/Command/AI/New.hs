{-# LANGUAGE DeriveGeneric #-}

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
    aiWriteWaspFile :: Path' Abs (Dir WaspProjectDir) -> String -> String -> Command Text
    aiWriteWaspFile absWaspProjectDir appTitle appDesc = do
      -- TODO: Tell GPT about Wasp in general, shortly.
      -- Also give it an example of a Wasp file that is pretty rich. It can even be done as a previous part of the conversation, so it has an example of what is good.
      -- Then, tell it to generate a Wasp file for the given prompt, while also adding comments in it for every ExtImport, where comments explain what that file is supposed to contain / do, so basically to serve as instrutioncs to itself.
      -- Or probably best to tell it to provide those instructions separately, in a JSON object where key is the name of each page or whatever.
      -- Once it does, let's feed that Wasp file to the Wasp analyzer and see if it returns any errors. If it does, send it back to chat GPT for repairs.
      -- Finally, write it to disk.
      -- In the example wasp file, we can put in a lot of comments explaining stuff, but then we can ask it to not produce those once it produces a wasp file, so we save some tokens.
      -- We should also make it clear which feature in wasp file is related to which part of the prompt, so it can know to skip them properly.
      -- TODO: What if it fails to repair it? Well we can just pretend all is ok, let user fix it.
      -- TODO: As chatGPT to compress our prompt for us.
      let chatMessages =
            [ ChatMessage {role = System, content = "You are an expert Wasp developer, helping set up a new Wasp project."},
              ChatMessage
                { role = User,
                  content =
                    "Wasp is a framework for building web apps with React (frontend), NodeJS (backend) and Prisma (db)."
                      <> "\nIt let's you define high-level overview of your app via specialized Wasp DSL in main.wasp file, and then implements the details in JS/JSX files."
                      <> "\nMain Wasp features are: frontend routes and pages, operations (queries and actions -> it is really an RPC system), entities (data models), jobs (cron / scheduled), ... ."
                      <> "\nHere is an example main.wasp file:"
                      <> ""
                }
            ]
      error "TODO"

    aiWriteWaspPages :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspPages absWaspProjectDir waspFileContent = do
      -- TODO: Actually it should recieve AppSpec I think, right?
      -- So idea is here that for each page we give Wasp file to Chat GPT, also all the basic knowledge about Wasp,
      -- and then ask it to generate JS for that page. If page already exists, we also pass that and tell it to add stuff to it.
      -- How do we get comments to it about it? Well, maybe it is smart enough to pick them up from the wasp file?
      -- Or, we give them separately, but then we need them in a good format. I think it will be able to pick them up on its own.
      -- Oh and we also need to give it info about the concept of the page itself! And example. Uff.
      -- Maybe that is too much and we can't give it all that. In that case we should drop the idea of passing whole Wasp file,
      -- and we need to give it only instructions for that page. We can have it write those instructions separately, for each page, in that case. Yeah probably that is the best.
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
  putStrLn answer

-- TODO: We will need to have this on a server somewhere, not here, if we want to use our API keys.
--   If we let them use their API keys then it can be here.
queryChatGpt :: String -> [ChatMessage] -> IO String
queryChatGpt apiKey requestMessages = do
  -- TODO: We could try playing with parameters like temperature, max_tokens, ... .
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
    content :: !String
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
