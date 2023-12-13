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
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException), SomeException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT (runStateT), gets, modify)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP
import System.IO (hPutStrLn, stderr)
import UnliftIO (Handler (Handler), catches, throwIO)
import Wasp.AI.OpenAI (OpenAIApiKey)
import Wasp.AI.OpenAI.ChatGPT (ChatGPTParams (..), ChatMessage, ChatResponse)
import qualified Wasp.AI.OpenAI.ChatGPT as ChatGPT
import qualified Wasp.Util as Util
import Wasp.Util.IO.Retry (MonadRetry)
import qualified Wasp.Util.IO.Retry as R
import Wasp.Util.Network.HTTP (catchRetryableHttpException)
import qualified Wasp.Util.Network.HTTP as Utils.HTTP

newtype CodeAgent a = CodeAgent {_unCodeAgent :: ReaderT CodeAgentConfig (StateT CodeAgentState IO) a}
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader CodeAgentConfig, MonadState CodeAgentState)

data CodeAgentConfig = CodeAgentConfig
  { _openAIApiKey :: !OpenAIApiKey,
    _writeFile :: !(FilePath -> Text -> IO ()), -- TODO: Use StrongPath? Not clear which kind of path is it, rel, abs, ... .
    _writeLog :: !(Text -> IO ())
  }

instance MonadRetry CodeAgent where
  rThreadDelay = liftIO . threadDelay

runCodeAgent :: CodeAgentConfig -> CodeAgent a -> IO a
runCodeAgent config codeAgent =
  (fst <$> (_unCodeAgent codeAgent `runReaderT` config) `runStateT` initialState)
    `catches` [ Handler
                  ( \(e :: HTTP.HttpException) -> do
                      let errorInfo =
                            maybe (showShortException e) show $ Utils.HTTP.getHttpExceptionStatusCode e
                          logMsg = T.pack $ "Code agent failed with the http error: " <> errorInfo
                      _writeLog config logMsg
                      throwIO e
                  ),
                Handler
                  ( \(e :: SomeException) -> do
                      _writeLog config $
                        "Code agent failed with the following error: " <> T.pack (showShortException e)
                      throwIO e
                  )
              ]
  where
    initialState =
      CodeAgentState
        { _files = H.empty,
          _usage = [],
          _isGpt4Available = Nothing
        }

    shortenWithEllipsisTo maxLen text = if length text <= maxLen then text else (take maxLen text) <> "..."

    showShortException :: forall e. Exception e => e -> String
    showShortException = shortenWithEllipsisTo 30 . displayException

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
  key <- asks _openAIApiKey
  chatResponse <- queryChatGPTWithRetry key params messages
  modify $ \s -> s {_usage = _usage s <> [ChatGPT.usage chatResponse]}
  return $ ChatGPT.getChatResponseContent chatResponse
  where
{- ORMOLU_DISABLE -}
    queryChatGPTWithRetry :: OpenAIApiKey -> ChatGPTParams -> [ChatMessage] -> CodeAgent ChatResponse
    queryChatGPTWithRetry key params' messages' =
      do
        R.retry
          (R.expPause $ fromIntegral $ Util.secondsToMicroSeconds 10)
          3
          ( liftIO $
              (Right <$> ChatGPT.queryChatGPT key params' messages')
                `catchRetryableHttpException` ( \e -> do
                    hPutStrLn stderr $ "Caught retryable HTTP exception while doing ChatGPT request: "
                        <> maybe "" (\code -> "Status code: " <> show code <> "; ") (Utils.HTTP.getHttpExceptionStatusCode e)
                        <> show e
                    return $ Left e
                                              )
          )
          >>= either throwIO pure
{- ORMOLU_ENABLE -}

getOpenAIApiKey :: CodeAgent OpenAIApiKey
getOpenAIApiKey = asks _openAIApiKey

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
