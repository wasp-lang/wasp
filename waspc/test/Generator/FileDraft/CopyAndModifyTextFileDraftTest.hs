module Generator.FileDraft.CopyAndModifyTextFileDraftTest where

import Fixtures (systemSPRoot)
import qualified Generator.MockWriteableMonad as Mock
import StrongPath (parent, reldir, relfile, toFilePath, (</>))
import Test.Tasty.Hspec
import Wasp.Generator.FileDraft

spec_CopyAndModifyTextFileDraft :: Spec
spec_CopyAndModifyTextFileDraft = do
  describe "write" $ do
    it "Creates new file by copying existing text file and applying modifications to it" $ do
      let mock = write dstDir fileDraft
      let mockLogs = Mock.getMockLogs mock Mock.defaultMockConfig
      Mock.createDirectoryIfMissing_calls mockLogs
        `shouldBe` [(True, toFilePath $ parent expectedDstPath)]
      Mock.readFileAsText_calls mockLogs
        `shouldBe` [toFilePath expectedSrcPath]
      Mock.writeFileFromText_calls mockLogs
        `shouldBe` [(toFilePath expectedDstPath, "First line\nMock text file content")]
  where
    dstDir = systemSPRoot </> [reldir|a/b|]
    dstPath = [relfile|c/d/dst.txt|]
    srcPath = systemSPRoot </> [relfile|e/src.txt|]
    fileDraft = createCopyAndModifyTextFileDraft dstPath srcPath ("First line\n" <>)
    expectedSrcPath = srcPath
    expectedDstPath = dstDir </> dstPath
