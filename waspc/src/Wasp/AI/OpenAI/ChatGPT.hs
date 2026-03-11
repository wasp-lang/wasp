{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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
  )
where

import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 as BSU
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import Wasp.AI.OpenAI (OpenAIApiKey)
import qualified Wasp.Util as Util
import Wasp.Util.Debug (debugTrace)
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

  debugTrace
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

data ChatGPTParams = ChatGPTParams
  { _model :: !Model,
    _temperature :: !(Maybe Float)
    -- TODO: There are more parameters that chatGPT supports,
    --   we just didn't add them for now!
    --   Check https://platform.openai.com/docs/api-reference/completions/create for complete list.
  }
  deriving (Show)

-- | Accepts any model name string, letting the OpenAI API validate it.
newtype Model = Model String
  deriving (Eq)

instance Show Model where
  show (Model name) = name

instance FromJSON Model where
  parseJSON = Aeson.withText "Model" $ \t ->
    pure $ Model (T.unpack t)

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
