{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.AI.CodeAgent
  ( CodeAgent,
    CodeAgentConfig (..),
    runCodeAgent,
    writeToLog,
    writeToFile,
    writeNewFile,
    getFile,
    getAllFiles,
    queryChatGPT,
    getTotalTokensUsage,
    getOpenAIApiKey,
    checkIfGpt4IsAvailable,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import Wasp.AI.OpenAI (OpenAIApiKey)
import Wasp.AI.OpenAI.ChatGPT (ChatGPTParams (..), ChatMessage)
import qualified Wasp.AI.OpenAI.ChatGPT as ChatGPT

newtype CodeAgent a = CodeAgent {_unCodeAgent :: ReaderT CodeAgentConfig (StateT CodeAgentState IO) a}
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader CodeAgentConfig, MonadState CodeAgentState)

data CodeAgentConfig = CodeAgentConfig
  { _openAIApiKey :: !OpenAIApiKey,
    _writeFile :: !(FilePath -> Text -> IO ()), -- TODO: Use StrongPath? Not clear which kind of path is it, rel, abs, ... .
    _writeLog :: !(Text -> IO ()),
    _useGpt3IfGpt4NotAvailable :: !Bool
  }

runCodeAgent :: CodeAgentConfig -> CodeAgent a -> IO a
runCodeAgent config codeAgent =
  fst <$> (_unCodeAgent codeAgent `runReaderT` config) `runStateT` initialState
  where
    initialState =
      CodeAgentState
        { _files = H.empty,
          _usage = [],
          _isGpt4Available = Nothing
        }

writeToLog :: Text -> CodeAgent ()
writeToLog msg = asks _writeLog >>= \f -> liftIO $ f msg

writeToFile :: FilePath -> (Maybe Text -> Text) -> CodeAgent ()
writeToFile path updateContentFn = do
  content <- updateContentFn <$> getFile path
  asks _writeFile >>= \f -> liftIO $ f path content
  modify $ \s -> s {_files = H.insert path content (_files s)}

writeNewFile :: (FilePath, Text) -> CodeAgent ()
writeNewFile (path, content) =
  writeToFile path (maybe content $ error $ "file " <> path <> " shouldn't already exist")

getFile :: FilePath -> CodeAgent (Maybe Text)
getFile path = gets $ H.lookup path . _files

getAllFiles :: CodeAgent [(FilePath, Text)]
getAllFiles = gets $ H.toList . _files

queryChatGPT :: ChatGPTParams -> [ChatMessage] -> CodeAgent Text
queryChatGPT params messages = do
  params' <- do
    useGpt3IfGpt4NotAvailable <- asks _useGpt3IfGpt4NotAvailable
    if ChatGPT._model params == ChatGPT.GPT_4 && useGpt3IfGpt4NotAvailable
      then do
        isAvailable <- checkIfGpt4IsAvailable
        if not isAvailable
          then return $ params {ChatGPT._model = ChatGPT.GPT_3_5_turbo_16k}
          else return params
      else return params

  key <- asks _openAIApiKey
  chatResponse <- liftIO $ ChatGPT.queryChatGPT key params' messages
  modify $ \s -> s {_usage = _usage s <> [ChatGPT.usage chatResponse]}
  return $ ChatGPT.getChatResponseContent chatResponse

getOpenAIApiKey :: CodeAgent OpenAIApiKey
getOpenAIApiKey = asks _openAIApiKey

checkIfGpt4IsAvailable :: CodeAgent Bool
checkIfGpt4IsAvailable = do
  gets _isGpt4Available >>= \case
    Just isAvailable -> pure isAvailable
    Nothing -> do
      key <- asks _openAIApiKey
      isAvailable <- liftIO $ ChatGPT.checkIfGpt4IsAvailable key
      modify $ \s -> s {_isGpt4Available = Just isAvailable}
      return isAvailable

type NumTokens = Int

-- | Returns total tokens usage: (<num_prompt_tokens>, <num_completion_tokens>).
getTotalTokensUsage :: CodeAgent (NumTokens, NumTokens)
getTotalTokensUsage = do
  usage <- gets _usage
  let numPromptTokens = sum $ ChatGPT.prompt_tokens <$> usage
  let numCompletionTokens = sum $ ChatGPT.completion_tokens <$> usage
  return (numPromptTokens, numCompletionTokens)

data CodeAgentState = CodeAgentState
  { _files :: !(H.HashMap FilePath Text), -- TODO: Name this "cacheFiles" maybe?
    _usage :: ![ChatGPT.ChatResponseUsage],
    _isGpt4Available :: !(Maybe Bool)
  }
