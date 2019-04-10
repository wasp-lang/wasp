{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Generator.MockFileDraftIO
       ( MockFdIO
       , MockFdIOLogs(..)
       , MockFdIOConfig(..)
       , getMockLogs
       , defaultMockConfig
       ) where

import System.FilePath (FilePath, (</>))
import Data.Text (Text, pack)
import Control.Monad.State
import Data.Aeson as Aeson

import Generator.FileDraft
import Generator.FileDraft.FileDraftIO


-- TODO: Instead of manually defining mock like this, consider using monad-mock package,
--   it should do most of this automatically, now there is a lot of boilerplate.
--   Or we ourselves can maybe use template haskell to reduce duplication.

defaultMockConfig :: MockFdIOConfig
defaultMockConfig = MockFdIOConfig
    { getTemplatesDirAbsPath_impl = "mock/templates/dir"
    , getTemplateFileAbsPath_impl = \path -> "mock/templates/dir" </> path
    , compileAndRenderTemplate_impl = \path json -> (pack "Mock template content")
    }

getMockLogs :: MockFdIO a -> MockFdIOConfig -> MockFdIOLogs
getMockLogs mock config = fst $ execState (unMockFdIO mock) (emptyLogs, config)
  where
    emptyLogs = MockFdIOLogs [] [] [] [] [] []

instance FileDraftIO MockFdIO where
    writeFileFromText dstPath text = MockFdIO $ do
        modifyLogs (writeFileFromText_addCall dstPath text)

    getTemplatesDirAbsPath = MockFdIO $ do
        modifyLogs getTemplatesDirAbsPath_addCall
        (_, config) <- get
        return $ getTemplatesDirAbsPath_impl config

    createDirectoryIfMissing createParents path = MockFdIO $ do
        modifyLogs (createDirectoryIfMissing_addCall createParents path)

    copyFile srcPath dstPath = MockFdIO $ do
        modifyLogs (copyFile_addCall srcPath dstPath)

    getTemplateFileAbsPath path = MockFdIO $ do
        modifyLogs (getTemplateFileAbsPath_addCall path)
        (_, config) <- get
        return $ (getTemplateFileAbsPath_impl config) path

    compileAndRenderTemplate path json = MockFdIO $ do
        modifyLogs (compileAndRenderTemplate_addCall path json)
        (_, config) <- get
        return $ (compileAndRenderTemplate_impl config) path json

modifyLogs f = modify (\(logs, config) -> (f logs, config))

newtype MockFdIO a = MockFdIO { unMockFdIO :: State (MockFdIOLogs, MockFdIOConfig) a }
    deriving (Monad, Applicative, Functor)

data MockFdIOLogs = MockFdIOLogs
    { writeFileFromText_calls :: [(FilePath, Text)]
    , getTemplatesDirAbsPath_calls :: [()]
    , createDirectoryIfMissing_calls :: [(Bool, FilePath)]
    , copyFile_calls :: [(FilePath, FilePath)]
    , getTemplateFileAbsPath_calls :: [(FilePath)]
    , compileAndRenderTemplate_calls :: [(FilePath, Aeson.Value)]
    }

data MockFdIOConfig = MockFdIOConfig
    { getTemplatesDirAbsPath_impl :: FilePath
    , getTemplateFileAbsPath_impl :: FilePath -> FilePath
    , compileAndRenderTemplate_impl :: FilePath -> Aeson.Value -> Text
    }

writeFileFromText_addCall path text logs =
    logs { writeFileFromText_calls = (path, text):(writeFileFromText_calls logs) }

getTemplatesDirAbsPath_addCall logs =
    logs { getTemplatesDirAbsPath_calls = ():(getTemplatesDirAbsPath_calls logs) }

getTemplateFileAbsPath_addCall path logs =
    logs { getTemplateFileAbsPath_calls = (path):(getTemplateFileAbsPath_calls logs) }

copyFile_addCall srcPath dstPath logs =
    logs { copyFile_calls = (srcPath, dstPath):(copyFile_calls logs) }

createDirectoryIfMissing_addCall createParents path logs =
    logs { createDirectoryIfMissing_calls =
           (createParents, path):(createDirectoryIfMissing_calls logs) }

compileAndRenderTemplate_addCall path json logs =
    logs { compileAndRenderTemplate_calls =
           (path, json):(compileAndRenderTemplate_calls logs) }
