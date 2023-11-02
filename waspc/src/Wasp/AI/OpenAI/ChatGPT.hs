{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AI.OpenAI.ChatGPT
  ( queryChatGPT,
    ChatGPTParams (..),
    Model (..),
    ChatResponse (..),
    ChatResponseUsage (..),
    ChatResponseChoice (..),
    ChatMessage (..),
    ChatRole (..),
    getChatResponseContent,
    checkIfGpt4IsAvailable,
  )
where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 as BSU
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Pretty.Simple (pTrace)
import GHC.Generics (Generic)
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import Wasp.AI.OpenAI (OpenAIApiKey)
import qualified Wasp.Util as Util
import qualified Wasp.Util.Network.HTTP as Utils.HTTP

-- | Might throw an HttpException.
queryChatGPT :: OpenAIApiKey -> ChatGPTParams -> [ChatMessage] -> IO ChatResponse
queryChatGPT apiKey params requestMessages = do
  let reqBodyJson =
        Aeson.object $
          [ "model" .= show (_model params),
            "messages" .= requestMessages
          ]
            <> ["temperature" .= t | Just t <- pure $ _temperature params]
      request =
        -- 90 seconds should be more than enough for ChatGPT to generate an answer, or reach its own timeout.
        -- If it proves in the future that it might need more time, we can increase this number.
        HTTP.setRequestResponseTimeout (HTTP.C.responseTimeoutMicro $ Util.secondsToMicroSeconds 300) $
          HTTP.setRequestHeader "Authorization" [BSU.fromString $ "Bearer " <> apiKey] $
            HTTP.setRequestBodyJSON reqBodyJson $
              HTTP.parseRequest_ "POST https://api.openai.com/v1/chat/completions"

  (chatResponse :: ChatResponse) <-
    Utils.HTTP.httpJSONThatThrowsIfNot2xx request
      <&> either (error . ("Failed to parse ChatGPT response body as JSON: " <>)) Prelude.id

  when True $
    pTrace
      ( "\n\n\n\n==================================\n"
          <> "\n===== GPT PARAMS ======\n"
          <> show params
          <> "\n====== REQUEST =======\n"
          <> show requestMessages
          <> "\n====== RESPONSE ======\n"
          <> show chatResponse
          <> "\n==================================\n\n\n\n"
      )
      $ return ()

  return chatResponse

getChatResponseContent :: ChatResponse -> Text
getChatResponseContent = content . message . head . choices

checkIfGpt4IsAvailable :: OpenAIApiKey -> IO Bool
checkIfGpt4IsAvailable apiKey = do
  let request =
        HTTP.setRequestHeader "Authorization" [BSU.fromString $ "Bearer " <> apiKey] $
          HTTP.parseRequest_ $ "GET https://api.openai.com/v1/models/" <> show GPT_4
  (200 ==) . HTTP.getResponseStatusCode <$> HTTP.httpNoBody request

data ChatGPTParams = ChatGPTParams
  { _model :: !Model,
    _temperature :: !(Maybe Float)
    -- TODO: There are more parameters that chatGPT supports,
    --   we just didn't add them for now!
    --   Check https://platform.openai.com/docs/api-reference/completions/create for complete list.
  }
  deriving (Show)

-- TODO: There are some more data models there but for now we went with these core ones.
data Model = GPT_3_5_turbo | GPT_3_5_turbo_16k | GPT_4
  deriving (Eq, Bounded, Enum)

instance Show Model where
  show = modelOpenAiId

modelOpenAiId :: Model -> String
modelOpenAiId = \case
  GPT_3_5_turbo -> "gpt-3.5-turbo"
  GPT_3_5_turbo_16k -> "gpt-3.5-turbo-16k"
  GPT_4 -> "gpt-4"

instance FromJSON Model where
  parseJSON = Aeson.withText "Model" $ \t ->
    let t' = T.unpack t
        models = [minBound .. maxBound]
     in find ((== t') . modelOpenAiId) models
          & maybe (fail $ "Invalid GPT model: " <> t') pure

data ChatResponse = ChatResponse
  { id :: !String,
    object :: !String,
    created :: !Int,
    model :: !String,
    choices :: ![ChatResponseChoice],
    usage :: !ChatResponseUsage
  }
  deriving (Generic, Show, FromJSON)

data ChatResponseUsage = ChatResponseUsage
  { prompt_tokens :: !Int,
    completion_tokens :: !Int,
    total_tokens :: !Int
  }
  deriving (Generic, Show, FromJSON)

data ChatResponseChoice = ChatResponseChoice
  { index :: !Int,
    message :: !ChatMessage,
    finish_reason :: !String
  }
  deriving (Generic, Show, FromJSON)

data ChatMessage = ChatMessage
  { role :: !ChatRole,
    content :: !Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data ChatRole = User | System | Assistant
  deriving (Generic, Show)

instance ToJSON ChatRole where
  toJSON User = "user"
  toJSON System = "system"
  toJSON Assistant = "assistant"

instance FromJSON ChatRole where
  parseJSON = Aeson.withText "ChatRole" $ \case
    "user" -> return User
    "system" -> return System
    "assistant" -> return Assistant
    other -> fail $ "Invalid ChatRole: " <> show other
