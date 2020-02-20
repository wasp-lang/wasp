module Generator.FileDraft.CopyFileDraftTest where

import Test.Tasty.Hspec

import qualified Path
import Path ((</>), absdir, relfile, absfile)

import Generator.FileDraft

import qualified Generator.MockWriteableMonad as Mock


spec_CopyFileDraft :: Spec
spec_CopyFileDraft = do
    describe "write" $ do
        it "Creates new file by copying existing file" $ do
            let mock = write dstDir fileDraft
            let mockLogs = Mock.getMockLogs mock Mock.defaultMockConfig
            Mock.createDirectoryIfMissing_calls mockLogs
                `shouldBe` [(True, Path.toFilePath $ Path.parent expectedDstPath)]
            Mock.copyFile_calls mockLogs
                `shouldBe` [(Path.toFilePath expectedSrcPath, Path.toFilePath expectedDstPath)]
              where
                (dstDir, dstPath, srcPath) = ([absdir|/a/b|], [relfile|c/d/dst.txt|], [absfile|/e/src.txt|])
                fileDraft = createCopyFileDraft dstPath srcPath
                expectedSrcPath = srcPath
                expectedDstPath = dstDir </> dstPath
