{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generator.MockWriteableMonad
       ( MockWriteableMonad
       , MockWriteableMonadLogs(..)
       , MockWriteableMonadConfig(..)
       , getMockLogs
       , defaultMockConfig
       ) where

import Data.Text (Text, pack)
import Control.Monad.State
import qualified Data.Aeson as Aeson
import Path ((</>), absdir)
import qualified Path.Aliases as Path

import Generator.FileDraft.WriteableMonad


-- TODO: Instead of manually defining mock like this, consider using monad-mock package,
--   it should do most of this automatically, now there is a lot of boilerplate.
--   Or we ourselves can maybe use template haskell to reduce duplication.

defaultMockConfig :: MockWriteableMonadConfig
defaultMockConfig = MockWriteableMonadConfig
    { getTemplatesDirAbsPath_impl = [absdir|/mock/templates/dir|]
    , getTemplateFileAbsPath_impl = \path -> [absdir|/mock/templates/dir|] </> path
    , compileAndRenderTemplate_impl = \_ _ -> (pack "Mock template content")
    }

getMockLogs :: MockWriteableMonad a -> MockWriteableMonadConfig -> MockWriteableMonadLogs
getMockLogs mock config = fst $ execState (unMockWriteableMonad mock) (emptyLogs, config)
  where
    emptyLogs = MockWriteableMonadLogs [] [] [] [] [] []

instance WriteableMonad MockWriteableMonad where
    writeFileFromText dstPath text = MockWriteableMonad $ do
        modifyLogs (writeFileFromText_addCall dstPath text)

    getTemplatesDirAbsPath = MockWriteableMonad $ do
        modifyLogs getTemplatesDirAbsPath_addCall
        (_, config) <- get
        return $ getTemplatesDirAbsPath_impl config

    createDirectoryIfMissing createParents path = MockWriteableMonad $ do
        modifyLogs (createDirectoryIfMissing_addCall createParents path)

    copyFile srcPath dstPath = MockWriteableMonad $ do
        modifyLogs (copyFile_addCall srcPath dstPath)

    getTemplateFileAbsPath path = MockWriteableMonad $ do
        modifyLogs (getTemplateFileAbsPath_addCall path)
        (_, config) <- get
        return $ (getTemplateFileAbsPath_impl config) path

    compileAndRenderTemplate path json = MockWriteableMonad $ do
        modifyLogs (compileAndRenderTemplate_addCall path json)
        (_, config) <- get
        return $ (compileAndRenderTemplate_impl config) path json

modifyLogs :: MonadState (a, b) m => (a -> a) -> m ()
modifyLogs f = modify (\(logs, config) -> (f logs, config))

newtype MockWriteableMonad a = MockWriteableMonad
    { unMockWriteableMonad :: State (MockWriteableMonadLogs, MockWriteableMonadConfig) a
    }
    deriving (Monad, Applicative, Functor)

data MockWriteableMonadLogs = MockWriteableMonadLogs
    { writeFileFromText_calls :: [(FilePath, Text)]
    , getTemplatesDirAbsPath_calls :: [()]
    , createDirectoryIfMissing_calls :: [(Bool, FilePath)]
    , copyFile_calls :: [(FilePath, FilePath)]
    , getTemplateFileAbsPath_calls :: [(Path.RelFile)]
    , compileAndRenderTemplate_calls :: [(Path.RelFile, Aeson.Value)]
    }

data MockWriteableMonadConfig = MockWriteableMonadConfig
    { getTemplatesDirAbsPath_impl :: Path.AbsDir
    , getTemplateFileAbsPath_impl :: Path.RelFile -> Path.AbsFile
    , compileAndRenderTemplate_impl :: Path.RelFile -> Aeson.Value -> Text
    }

writeFileFromText_addCall :: FilePath -> Text -> MockWriteableMonadLogs -> MockWriteableMonadLogs
writeFileFromText_addCall path text logs =
    logs { writeFileFromText_calls = (path, text):(writeFileFromText_calls logs) }

getTemplatesDirAbsPath_addCall :: MockWriteableMonadLogs -> MockWriteableMonadLogs
getTemplatesDirAbsPath_addCall logs =
    logs { getTemplatesDirAbsPath_calls = ():(getTemplatesDirAbsPath_calls logs) }

getTemplateFileAbsPath_addCall :: Path.RelFile -> MockWriteableMonadLogs -> MockWriteableMonadLogs
getTemplateFileAbsPath_addCall path logs =
    logs { getTemplateFileAbsPath_calls = (path):(getTemplateFileAbsPath_calls logs) }

copyFile_addCall :: FilePath -> FilePath -> MockWriteableMonadLogs -> MockWriteableMonadLogs
copyFile_addCall srcPath dstPath logs =
    logs { copyFile_calls = (srcPath, dstPath):(copyFile_calls logs) }

createDirectoryIfMissing_addCall :: Bool -> FilePath -> MockWriteableMonadLogs -> MockWriteableMonadLogs
createDirectoryIfMissing_addCall createParents path logs =
    logs { createDirectoryIfMissing_calls =
           (createParents, path):(createDirectoryIfMissing_calls logs) }

compileAndRenderTemplate_addCall :: Path.RelFile -> Aeson.Value -> MockWriteableMonadLogs -> MockWriteableMonadLogs
compileAndRenderTemplate_addCall path json logs =
    logs { compileAndRenderTemplate_calls =
           (path, json):(compileAndRenderTemplate_calls logs) }
