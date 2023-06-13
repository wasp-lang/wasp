{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wasp.Cli.Command.AI.CodeAgent
  ( CodeAgent,
    CodeAgentConfig (..),
    runCodeAgent,
    writeToLog,
    writeToFile,
    writeNewFile,
    getFile,
    getAllFiles,
    queryChatGPT,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState, StateT (runStateT), gets)
import Data.List (find)
import Data.Text (Text)
import Wasp.OpenAI (OpenAIApiKey)
import Wasp.OpenAI.ChatGPT (ChatGPTParams, ChatMessage)
import qualified Wasp.OpenAI.ChatGPT as ChatGPT

newtype CodeAgent a = CodeAgent {_unCodeAgent :: ReaderT CodeAgentConfig (StateT CodeAgentState IO) a}
  deriving (Monad, Applicative, Functor, MonadIO, MonadReader CodeAgentConfig, MonadState CodeAgentState)

data CodeAgentConfig = CodeAgentConfig
  { _openAIApiKey :: !OpenAIApiKey,
    _writeFile :: !(FilePath -> Text -> IO ()), -- TODO: Use StrongPath? Not clear which kind of path is it, rel, abs, ... .
    _writeLog :: !(Text -> IO ())
  }

runCodeAgent :: CodeAgent a -> CodeAgentConfig -> IO a
runCodeAgent codeAgent config =
  fst <$> (_unCodeAgent codeAgent `runReaderT` config) `runStateT` initialState
  where
    initialState = CodeAgentState {_files = []}

writeToLog :: Text -> CodeAgent ()
writeToLog msg = asks _writeLog >>= \f -> liftIO $ f msg

writeToFile :: FilePath -> (Maybe Text -> Text) -> CodeAgent ()
writeToFile path updateContentFn = do
  content <- updateContentFn <$> getFile path
  asks _writeFile >>= \f -> liftIO $ f path content

writeNewFile :: (FilePath, Text) -> CodeAgent ()
writeNewFile (path, content) =
  writeToFile path (maybe content $ error $ "file " <> path <> " shouldn't already exist")

getFile :: FilePath -> CodeAgent (Maybe Text)
getFile path = (snd <$>) . find ((== path) . fst) <$> getAllFiles

getAllFiles :: CodeAgent [(FilePath, Text)]
getAllFiles = gets _files

queryChatGPT :: ChatGPTParams -> [ChatMessage] -> CodeAgent Text
queryChatGPT params messages = do
  key <- asks _openAIApiKey
  liftIO $ ChatGPT.queryChatGPT key params messages

data CodeAgentState = CodeAgentState
  { _files :: ![(FilePath, Text)]
  }
