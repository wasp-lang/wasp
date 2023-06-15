module Wasp.Cli.Command.AI.GenerateNewProject.Common
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
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent, queryChatGPT, writeToFile)
import Wasp.OpenAI.ChatGPT (ChatGPTParams (..), ChatMessage (..), Model (GPT_3_5_turbo_16k))
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
queryChatGPTForJSON chatGPTParams chatMessages = do
  responseJSONText <- naiveTrimJSON <$> queryChatGPT chatGPTParams chatMessages
  case Aeson.eitherDecode $ textToLazyBS responseJSONText of
    Right plan -> return plan
    Left _errMsg ->
      -- TODO: Handle this better.
      --   Try sending response back to chatGPT and ask it to fix it -> hey it is not valid JSON, fix it.
      --   If it fails nonetheless, write to log, to let user know.
      error $ "Failed to parse ChatGPT response as a JSON. Response:\n" <> T.unpack responseJSONText

-- TODO: Test more for the optimal temperature (possibly higher).
-- TODO: Should we make sure we have max_tokens set to high enough?
defaultChatGPTParams :: ChatGPTParams
defaultChatGPTParams = ChatGPTParams {_model = GPT_3_5_turbo_16k, _temperature = Just 1.0}

writeToWaspFileEnd :: FilePath -> Text -> CodeAgent ()
writeToWaspFileEnd waspFilePath text = do
  writeToFile waspFilePath $
    (<> "\n" <> text) . fromMaybe (error "wasp file shouldn't be empty")
