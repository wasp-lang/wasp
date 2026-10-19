module Generator.FileDraft.EmptyDirFileDraftTest where

import Fixtures.Paths (systemSPRoot)
import qualified Generator.MockWriteableMonad as Mock
import qualified StrongPath as SP
import Test.Hspec
import Wasp.Generator.FileDraft

spec_EmptyDirFileDraft :: Spec
spec_EmptyDirFileDraft = do
  describe "write" $ do
    it "Creates the dir with its parents" $ do
      let mock = write dstDir fileDraft
      let mockLogs = Mock.getMockLogs mock Mock.defaultMockConfig
      Mock.createDirectoryIfMissing_calls mockLogs
        `shouldBe` [(True, SP.toFilePath expectedDstPath)]
      Mock.writeFileFromText_calls mockLogs `shouldBe` []
  where
    (dstDir, dstPath) =
      ( systemSPRoot SP.</> [SP.reldir|a/b|],
        [SP.reldir|c/d|]
      )
    fileDraft = createEmptyDirFileDraft dstPath
    expectedDstPath = dstDir SP.</> dstPath
