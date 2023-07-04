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

import Control.Arrow ()
import Control.Monad (void, when)
import Data.Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.UTF8 as BSU
import Data.Text (Text)
import Debug.Pretty.Simple (pTrace)
import GHC.Generics (Generic)
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import UnliftIO.Exception (catch, throwIO)
import Wasp.AI.OpenAI (OpenAIApiKey)
import qualified Wasp.Util.IO.Retry as R

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
        HTTP.setRequestResponseTimeout (HTTP.C.responseTimeoutMicro $ secondsToMicroSeconds 90) $
          HTTP.setRequestHeader "Authorization" [BSU.fromString $ "Bearer " <> apiKey] $
            HTTP.setRequestBodyJSON reqBodyJson $
              HTTP.parseRequest_ "POST https://api.openai.com/v1/chat/completions"

  response <- httpJSONWithRetry request

  -- TODO: I should probably check status code here, confirm it is 200.
  let _responseStatusCode = HTTP.getResponseStatusCode response

  let (chatResponse :: ChatResponse) = HTTP.getResponseBody response

  when True $
    pTrace
      ( "\n\n\n\n==================================\n"
          <> "\n====== REQUEST =======\n"
          <> show requestMessages
          <> "\n======================\n"
          <> "\n====== RESPONSE ======\n"
          <> show chatResponse
          <> "\n======================\n"
          <> "\n==================================\n\n\n\n"
      )
      $ return ()

  return chatResponse
  where
    secondsToMicroSeconds :: Int -> Int
    secondsToMicroSeconds = (* 1000000)

    httpJSONWithRetry :: (FromJSON a) => HTTP.Request -> IO (HTTP.Response a)
    httpJSONWithRetry request =
      R.retry
        (R.expPause $ fromIntegral $ secondsToMicroSeconds 10)
        3
        ( ( do
              response <- HTTP.httpJSON request
              -- NOTE: Although docs say that httpJSON will will be thrown for all status codes except 200,
              --   I believe I saw situations where it wasn't thrown and instead it was part of response,
              --   so I added this extra handling here just in case. If we can prove that is not the case,
              --   and that exception is always thrown, we can remove this if-then-else.
              if shouldRetry response
                then pure $ Left $ HTTP.HttpExceptionRequest request $ HTTP.C.StatusCodeException (void response) ""
                else pure $ Right response
          )
            `catch` (\e@(HTTP.HttpExceptionRequest _req HTTP.C.ResponseTimeout) -> pure $ Left e)
            `catch` (\e@(HTTP.HttpExceptionRequest _req HTTP.C.ConnectionTimeout) -> pure $ Left e)
            `catch` ( \e@(HTTP.HttpExceptionRequest _req (HTTP.C.StatusCodeException response _)) ->
                        if shouldRetry response then pure $ Left e else throwIO e
                    )
        )
        >>= either throwIO pure
      where
        shouldRetry response =
          HTTP.getResponseStatusCode response `elem` retrayableHttpErrorStatusCodes
        retrayableHttpErrorStatusCodes = [503, 429, 408, 502, 504]

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

-- TODO: There are some more data models there but for now we went with these core ones.
data Model = GPT_3_5_turbo | GPT_3_5_turbo_16k | GPT_4
  deriving (Eq)

instance Show Model where
  show GPT_3_5_turbo = "gpt-3.5-turbo"
  show GPT_3_5_turbo_16k = "gpt-3.5-turbo-16k"
  show GPT_4 = "gpt-4"

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
