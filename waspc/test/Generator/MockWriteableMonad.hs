{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generator.MockWriteableMonad
  ( MockWriteableMonad,
    MockWriteableMonadLogs (..),
    MockWriteableMonadConfig (..),
    getMockLogs,
    defaultMockConfig,
  )
where

import Control.Monad.State
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import Fixtures (systemSPRoot)
import StrongPath (Abs, Dir, Dir', File', Path', Rel, reldir, (</>))
import Wasp.Generator.FileDraft.WriteableMonad
import Wasp.Generator.Templates (TemplatesDir)

-- TODO: Instead of manually defining mock like this, consider using monad-mock package,
--   it should do most of this automatically, now there is a lot of boilerplate.
--   Or we ourselves can maybe use template haskell to reduce duplication.

defaultMockConfig :: MockWriteableMonadConfig
defaultMockConfig =
  MockWriteableMonadConfig
    { getTemplatesDirAbsPath_impl = systemSPRoot </> [reldir|mock/templates/dir|],
      getTemplateFileAbsPath_impl = \path -> systemSPRoot </> [reldir|mock/templates/dir|] </> path,
      compileAndRenderTemplate_impl = \_ _ -> pack "Mock template content",
      doesFileExist_impl = const True,
      doesDirectoryExist_impl = const True
    }

getMockLogs :: MockWriteableMonad a -> MockWriteableMonadConfig -> MockWriteableMonadLogs
getMockLogs mock config = fst $ execState (unMockWriteableMonad mock) (emptyLogs, config)
  where
    emptyLogs = MockWriteableMonadLogs [] [] [] [] [] [] [] []

instance WriteableMonad MockWriteableMonad where
  writeFileFromText dstPath text = MockWriteableMonad $ do
    modifyLogs (writeFileFromText_addCall dstPath text)

  writeFileFromByteString dstPath b = MockWriteableMonad $ do
    modifyLogs (writeFileFromByteString_addCall dstPath b)

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
    return $ getTemplateFileAbsPath_impl config path

  compileAndRenderTemplate path json = MockWriteableMonad $ do
    modifyLogs (compileAndRenderTemplate_addCall path json)
    (_, config) <- get
    return $ compileAndRenderTemplate_impl config path json

  doesFileExist path = MockWriteableMonad $ do
    (_, config) <- get
    return $ doesFileExist_impl config path

  doesDirectoryExist path = MockWriteableMonad $ do
    (_, config) <- get
    return $ doesDirectoryExist_impl config path

  copyDirectoryRecursive srcPath dstPath = MockWriteableMonad $ do
    modifyLogs (copyDirectoryRecursive_addCall srcPath dstPath)

  throwIO = throwIO

instance MonadIO MockWriteableMonad where
  liftIO = undefined

modifyLogs :: MonadState (a, b) m => (a -> a) -> m ()
modifyLogs f = modify (first f)

newtype MockWriteableMonad a = MockWriteableMonad
  { unMockWriteableMonad :: State (MockWriteableMonadLogs, MockWriteableMonadConfig) a
  }
  deriving (Monad, Applicative, Functor)

data MockWriteableMonadLogs = MockWriteableMonadLogs
  { writeFileFromText_calls :: [(FilePath, Text)],
    writeFileFromByteString_calls :: [(FilePath, ByteString)],
    getTemplatesDirAbsPath_calls :: [()],
    createDirectoryIfMissing_calls :: [(Bool, FilePath)],
    copyFile_calls :: [(FilePath, FilePath)],
    getTemplateFileAbsPath_calls :: [Path' (Rel TemplatesDir) File'],
    compileAndRenderTemplate_calls :: [(Path' (Rel TemplatesDir) File', Aeson.Value)],
    copyDirectoryRecursive_calls :: [(Path' Abs Dir', Path' Abs Dir')]
  }

data MockWriteableMonadConfig = MockWriteableMonadConfig
  { getTemplatesDirAbsPath_impl :: Path' Abs (Dir TemplatesDir),
    getTemplateFileAbsPath_impl :: Path' (Rel TemplatesDir) File' -> Path' Abs File',
    compileAndRenderTemplate_impl :: Path' (Rel TemplatesDir) File' -> Aeson.Value -> Text,
    doesFileExist_impl :: FilePath -> Bool,
    doesDirectoryExist_impl :: FilePath -> Bool
  }

writeFileFromText_addCall :: FilePath -> Text -> MockWriteableMonadLogs -> MockWriteableMonadLogs
writeFileFromText_addCall path text logs =
  logs {writeFileFromText_calls = (path, text) : writeFileFromText_calls logs}

writeFileFromByteString_addCall :: FilePath -> ByteString -> MockWriteableMonadLogs -> MockWriteableMonadLogs
writeFileFromByteString_addCall path bs logs =
  logs {writeFileFromByteString_calls = (path, bs) : writeFileFromByteString_calls logs}

getTemplatesDirAbsPath_addCall :: MockWriteableMonadLogs -> MockWriteableMonadLogs
getTemplatesDirAbsPath_addCall logs =
  logs {getTemplatesDirAbsPath_calls = () : getTemplatesDirAbsPath_calls logs}

getTemplateFileAbsPath_addCall :: Path' (Rel TemplatesDir) File' -> MockWriteableMonadLogs -> MockWriteableMonadLogs
getTemplateFileAbsPath_addCall path logs =
  logs {getTemplateFileAbsPath_calls = path : getTemplateFileAbsPath_calls logs}

copyFile_addCall :: FilePath -> FilePath -> MockWriteableMonadLogs -> MockWriteableMonadLogs
copyFile_addCall srcPath dstPath logs =
  logs {copyFile_calls = (srcPath, dstPath) : copyFile_calls logs}

createDirectoryIfMissing_addCall :: Bool -> FilePath -> MockWriteableMonadLogs -> MockWriteableMonadLogs
createDirectoryIfMissing_addCall createParents path logs =
  logs
    { createDirectoryIfMissing_calls =
        (createParents, path) : createDirectoryIfMissing_calls logs
    }

copyDirectoryRecursive_addCall :: Path' Abs Dir' -> Path' Abs Dir' -> MockWriteableMonadLogs -> MockWriteableMonadLogs
copyDirectoryRecursive_addCall srcPath dstPath logs =
  logs {copyDirectoryRecursive_calls = (srcPath, dstPath) : copyDirectoryRecursive_calls logs}

compileAndRenderTemplate_addCall ::
  Path' (Rel TemplatesDir) File' ->
  Aeson.Value ->
  MockWriteableMonadLogs ->
  MockWriteableMonadLogs
compileAndRenderTemplate_addCall path json logs =
  logs
    { compileAndRenderTemplate_calls =
        (path, json) : compileAndRenderTemplate_calls logs
    }
