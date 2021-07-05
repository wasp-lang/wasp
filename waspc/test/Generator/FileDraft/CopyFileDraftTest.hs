module Generator.FileDraft.CopyFileDraftTest where

import Fixtures (systemSPRoot)
import Generator.FileDraft
import qualified Generator.MockWriteableMonad as Mock
import qualified StrongPath as SP
import Test.Tasty.Hspec

spec_CopyFileDraft :: Spec
spec_CopyFileDraft = do
  describe "write" $ do
    it "Creates new file by copying existing file" $ do
      let mock = write dstDir fileDraft
      let mockLogs = Mock.getMockLogs mock Mock.defaultMockConfig
      Mock.createDirectoryIfMissing_calls mockLogs
        `shouldBe` [(True, SP.toFilePath $ SP.parent expectedDstPath)]
      Mock.copyFile_calls mockLogs
        `shouldBe` [(SP.toFilePath expectedSrcPath, SP.toFilePath expectedDstPath)]
  where
    (dstDir, dstPath, srcPath) =
      ( systemSPRoot SP.</> [SP.reldir|a/b|],
        [SP.relfile|c/d/dst.txt|],
        systemSPRoot SP.</> [SP.relfile|e/src.txt|]
      )
    fileDraft = createCopyFileDraft dstPath srcPath
    expectedSrcPath = srcPath
    expectedDstPath = dstDir SP.</> dstPath
