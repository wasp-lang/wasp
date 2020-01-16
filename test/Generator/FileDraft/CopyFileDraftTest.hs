module Generator.FileDraft.CopyFileDraftTest where

import Test.Tasty.Hspec

import System.FilePath ((</>), takeDirectory)

import Generator.FileDraft

import qualified Generator.MockWriteableMonad as Mock


spec_CopyFileDraft :: Spec
spec_CopyFileDraft = do
    describe "write" $ do
        it "Creates new file by copying existing file" $ do
            let mock = write dstDir fileDraft
            let mockLogs = Mock.getMockLogs mock Mock.defaultMockConfig
            Mock.createDirectoryIfMissing_calls mockLogs
                `shouldBe` [(True, takeDirectory expectedDstPath)]
            Mock.copyFile_calls mockLogs
                `shouldBe` [(expectedSrcPath, expectedDstPath)]
              where
                (dstDir, dstPath, srcPath) = ("a/b", "c/d/dst.txt", "e/src.txt")
                fileDraft = createCopyFileDraft dstPath srcPath
                expectedSrcPath = srcPath
                expectedDstPath = dstDir </> dstPath
