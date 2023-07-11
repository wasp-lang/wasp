module Wasp.AI.GenerateNewProject.Common
  ( NewProjectDetails (..),
    NewProjectConfig (..),
    AuthProvider (..),
    File,
    getProjectAuth,
    getProjectPrimaryColor,
    emptyNewProjectConfig,
    queryChatGPTForJSON,
    defaultChatGPTParams,
    defaultChatGPTParamsForFixing,
    writeToWaspFileEnd,
  )
where

import Data.Aeson (FromJSON, withObject, withText, (.:?))
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
    _projectConfig :: NewProjectConfig
  }

data NewProjectConfig = NewProjectConfig
  { projectAuth :: !(Maybe AuthProvider),
    -- One of the Tailwind color names: https://tailwindcss.com/docs/customizing-colors
    projectPrimaryColor :: !(Maybe String),
    projectDefaultGptModel :: !(Maybe GPT.Model),
    projectDefaultGptTemperature :: !(Maybe Float)
  }
  deriving (Show)

instance Aeson.FromJSON NewProjectConfig where
  parseJSON = withObject "NewProjectConfig" $ \obj -> do
    auth <- obj .:? "auth"
    primaryColor <- obj .:? "primaryColor"
    defaultGptModel <- obj .:? "defaultGptModel"
    defaultGptTemperature <- obj .:? "defaultGptTemperature"
    return
      ( NewProjectConfig
          { projectAuth = auth,
            projectPrimaryColor = primaryColor,
            projectDefaultGptModel = defaultGptModel,
            projectDefaultGptTemperature = defaultGptTemperature
          }
      )

emptyNewProjectConfig :: NewProjectConfig
emptyNewProjectConfig =
  NewProjectConfig
    { projectAuth = Nothing,
      projectPrimaryColor = Nothing,
      projectDefaultGptModel = Nothing,
      projectDefaultGptTemperature = Nothing
    }

getProjectAuth :: NewProjectDetails -> AuthProvider
getProjectAuth = fromMaybe UsernameAndPassword . projectAuth . _projectConfig

getProjectPrimaryColor :: NewProjectDetails -> String
getProjectPrimaryColor = fromMaybe "slate" . projectPrimaryColor . _projectConfig

-- TODO: Support more methods.
data AuthProvider = UsernameAndPassword
  deriving (Show)

instance Aeson.FromJSON AuthProvider where
  parseJSON = withText "AuthProvider" $ \case
    "UsernameAndPassword" -> return UsernameAndPassword
    _ -> fail "invalid auth provider"

-- TODO: Make these relative to WaspProjectDir, via StrongPath?
type File = (FilePath, Text)

queryChatGPTForJSON :: FromJSON a => ChatGPTParams -> [ChatMessage] -> CodeAgent a
queryChatGPTForJSON chatGPTParams initChatMsgs = doQueryForJSON 0 0 initChatMsgs
  where
    -- Retry logic here got a bit complex, here is a short explanation.
    -- We first try to do normal request, if that fails and returns invalid JSON, we ask chatGPT to
    -- fix it while continuing on the initial conversation.
    -- If GPT gives us invalid JSON for the `maxNumFailuresPerRunBeforeGivingUpOnARun`th time,
    -- we give up on a current conversation (aka run) and start a new one, from the scratch, with
    -- the initial request, to give GPT a chance to have a fresh start since obviously it can't fix
    -- the mistake it did.
    -- Once we fail `maxNumFailedRunsBeforeGivingUpCompletely`th conversation (run), we give up
    -- completely.
    -- So max total number of GPT requests is
    -- `maxNumFailedRunsBeforeGivingUpCompletely` * `maxNumFailuresPerRunBeforeGivingUpOnARun`.
    doQueryForJSON :: (FromJSON a) => Int -> Int -> [ChatMessage] -> CodeAgent a
    doQueryForJSON numPrevFailedRuns numPrevFailuresPerCurrentRun chatMsgs = do
      response <- queryChatGPT chatGPTParams chatMsgs
      case Aeson.eitherDecode . textToLazyBS . naiveTrimJSON $ response of
        Right result -> return result
        Left errMsg ->
          let numFailuresPerCurrentRun = numPrevFailuresPerCurrentRun + 1
           in if numFailuresPerCurrentRun >= maxNumFailuresPerRunBeforeGivingUpOnARun
                then do
                  let numFailedRuns = numPrevFailedRuns + 1
                   in if numFailedRuns >= maxNumFailedRunsBeforeGivingUpCompletely
                        then do
                          writeToLog givingUpMessage
                          error $ T.unpack givingUpMessage <> " Error:" <> errMsg
                        else do
                          writeToLog retryingMessage
                          doQueryForJSON numFailedRuns 0 initChatMsgs
                else do
                  writeToLog retryingMessage
                  doQueryForJSON numPrevFailedRuns numFailuresPerCurrentRun $
                    initChatMsgs
                      ++ [ GPT.ChatMessage {GPT.role = GPT.Assistant, GPT.content = response},
                           GPT.ChatMessage
                             { GPT.role = GPT.User,
                               GPT.content =
                                 "You did not respond with valid JSON. Please fix it and respond with only"
                                   <> " valid JSON, no other text or explanations. Error I got parsing JSON"
                                   <> " from your last message: "
                                   <> T.pack errMsg
                                   <> ". Newlines should be escaped as \\n."
                             }
                         ]
      where
        givingUpMessage = "Repeatedly failed to parse ChatGPT response as JSON, giving up."
        retryingMessage = "Failed to parse ChatGPT response as JSON, trying again."

    maxNumFailuresPerRunBeforeGivingUpOnARun = 2
    maxNumFailedRunsBeforeGivingUpCompletely = 2

defaultChatGPTParams :: NewProjectDetails -> ChatGPTParams
defaultChatGPTParams projectDetails =
  GPT.ChatGPTParams
    { GPT._model = fromMaybe GPT.GPT_3_5_turbo_16k (projectDefaultGptModel $ _projectConfig projectDetails),
      GPT._temperature = Just $ fromMaybe 0.7 (projectDefaultGptTemperature $ _projectConfig projectDetails)
    }

defaultChatGPTParamsForFixing :: NewProjectDetails -> ChatGPTParams
defaultChatGPTParamsForFixing projectDetails =
  let params = defaultChatGPTParams projectDetails
   in params {GPT._temperature = subtract 0.2 <$> GPT._temperature params}

writeToWaspFileEnd :: FilePath -> Text -> CodeAgent ()
writeToWaspFileEnd waspFilePath text = do
  writeToFile waspFilePath $
    (<> "\n" <> text) . fromMaybe (error "wasp file shouldn't be empty")
