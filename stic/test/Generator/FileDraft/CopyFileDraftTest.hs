module Generator.FileDraft.CopyFileDraftTest where

import qualified Test.Tasty
import Test.Tasty.Hspec

import System.FilePath ((</>), takeDirectory)

import Generator.FileDraft

import Generator.MockFileDraftIO


spec_CopyFileDraft :: Spec
spec_CopyFileDraft = do
    describe "writeToFile" $ do
        it "Creates new file by copying existing file" $ do
            let mock = writeToFile dstDir fileDraft
            let mockLogs = getMockLogs mock defaultMockConfig
            createDirectoryIfMissing_calls mockLogs
                `shouldBe` [(True, takeDirectory expectedDstPath)]
            copyFile_calls mockLogs
                `shouldBe` [(expectedSrcPath, expectedDstPath)]
              where
                (dstDir, dstPath, srcPath) = ("a/b", "c/d/dst.txt", "e/src.txt")
                fileDraft = createCopyFileDraft dstPath srcPath
                expectedSrcPath = mockTemplatesDirAbsPath </> srcPath
                expectedDstPath = dstDir </> dstPath
                mockTemplatesDirAbsPath = getTemplatesDirAbsPath_impl defaultMockConfig
