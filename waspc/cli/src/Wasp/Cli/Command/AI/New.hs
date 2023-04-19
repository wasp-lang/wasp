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
      error "TODO"

    aiWriteWaspPages :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspPages absWaspProjectDir waspFileContent = do
      error "TODO"

    aiWriteWaspOperations :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspOperations absWaspProjectDir waspFileContent = do
      error "TODO"

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
