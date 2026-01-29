module Generator.FileDraft.CopyLibDraftTest where

import Fixtures (systemSPRoot)
import qualified Generator.MockWriteableMonad as Mock
import qualified StrongPath as SP
import Test.Hspec
import Wasp.Generator.FileDraft

spec_CopyLibDraft :: Spec
spec_CopyLibDraft = do
  describe "write" $ do
    it "Creates new file by copying lib file from libs source dir" $ do
      let mock = write dstDir fileDraft
      let mockLogs = Mock.getMockLogs mock mockConfig
      Mock.getLibsSourceDirAbsPath_calls mockLogs
        `shouldBe` [()]
      Mock.createDirectoryIfMissing_calls mockLogs
        `shouldBe` [(True, SP.toFilePath $ SP.parent expectedDstPath)]
      Mock.copyFile_calls mockLogs
        `shouldBe` [(SP.toFilePath expectedSrcPath, SP.toFilePath expectedDstPath)]
  where
    (dstDir, dstPath, srcPathInLibsSourceDir) =
      ( systemSPRoot SP.</> [SP.reldir|a/b|],
        [SP.relfile|c/d/dst.tgz|],
        [SP.relfile|lib.tgz|]
      )
    fileDraft = createCopyLibFileDraft dstPath srcPathInLibsSourceDir
    mockLibsSourceDirAbsPath = systemSPRoot SP.</> [SP.reldir|mock/libs/dir|]
    expectedSrcPath = mockLibsSourceDirAbsPath SP.</> srcPathInLibsSourceDir
    expectedDstPath = dstDir SP.</> dstPath
    mockConfig =
      Mock.defaultMockConfig
        { Mock.getLibsSourceDirAbsPath_impl = mockLibsSourceDirAbsPath
        }
