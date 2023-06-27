module Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    File,
    AuthProvider (..),
    queryChatGPTForJSON,
    defaultChatGPTParams,
    writeToWaspFileEnd,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Wasp.AI.CodeAgent (CodeAgent, queryChatGPT, writeToFile, writeToLog)
import Wasp.AI.OpenAI.ChatGPT (ChatGPTParams, ChatMessage)
import qualified Wasp.AI.OpenAI.ChatGPT as GPT
import Wasp.Util (naiveTrimJSON, textToLazyBS)

data NewProjectDetails = NewProjectDetails
  { _projectAppName :: !String,
    _projectDescription :: !String,
    _projectAuth :: !AuthProvider
  }

-- TODO: Make these relative to WaspProjectDir, via StrongPath?
type File = (FilePath, Text)

-- TODO: Support more methods.
data AuthProvider = UsernameAndPassword

queryChatGPTForJSON :: FromJSON a => ChatGPTParams -> [ChatMessage] -> CodeAgent a
queryChatGPTForJSON chatGPTParams = doQueryForJSON 0
  where
    doQueryForJSON :: (FromJSON a) => Int -> [ChatMessage] -> CodeAgent a
    doQueryForJSON numPrevFailures chatMsgs = do
      response <- queryChatGPT chatGPTParams chatMsgs
      case Aeson.eitherDecode . textToLazyBS . naiveTrimJSON $ response of
        Right result -> return result
        Left errMsg ->
          let numFailures = numPrevFailures + 1
           in if numFailures <= maxNumFailuresBeforeGivingUp
                then
                  doQueryForJSON (numPrevFailures + 1) $
                    chatMsgs
                      ++ [ GPT.ChatMessage {GPT.role = GPT.Assistant, GPT.content = response},
                           GPT.ChatMessage
                             { GPT.role = GPT.User,
                               GPT.content =
                                 "You did not respond with valid JSON. Please fix it and respond with only"
                                   <> " valid JSON, no other text or explanations. Error I got parsing JSON"
                                   <> " from your last message: "
                                   <> T.pack errMsg
                             }
                         ]
                else do
                  writeToLog "Failed to parse ChatGPT response as JSON."
                  error $ "Failed to parse ChatGPT response as JSON: " <> errMsg

    maxNumFailuresBeforeGivingUp = 2

-- TODO: Test more for the optimal temperature (possibly higher).
defaultChatGPTParams :: ChatGPTParams
defaultChatGPTParams = GPT.ChatGPTParams {_model = GPT.GPT_3_5_turbo_16k, _temperature = Just 1.0}

writeToWaspFileEnd :: FilePath -> Text -> CodeAgent ()
writeToWaspFileEnd waspFilePath text = do
  writeToFile waspFilePath $
    (<> "\n" <> text) . fromMaybe (error "wasp file shouldn't be empty")
