module Generator.FileDraft.CopyFileDraftTest where

import           Test.Tasty.Hspec

import qualified Path                         as P

import           Generator.FileDraft
import qualified StrongPath                   as SP

import           Fixtures                     (systemPathRoot)
import qualified Generator.MockWriteableMonad as Mock


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
          ( SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|a/b|]
          , SP.fromPathRelFile [P.relfile|c/d/dst.txt|]
          , SP.fromPathAbsFile $ systemPathRoot P.</> [P.relfile|e/src.txt|]
          )
      fileDraft = createCopyFileDraft dstPath srcPath
      expectedSrcPath = srcPath
      expectedDstPath = dstDir SP.</> dstPath
