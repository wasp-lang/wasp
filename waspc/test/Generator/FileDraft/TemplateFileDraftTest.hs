module Generator.FileDraft.TemplateFileDraftTest where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Fixtures (systemPathRoot)
import Generator.FileDraft
import qualified Generator.MockWriteableMonad as Mock
import qualified Path as P
import qualified StrongPath as SP
import Test.Tasty.Hspec

spec_TemplateFileDraft :: Spec
spec_TemplateFileDraft = do
  describe "write" $ do
    it "Creates new file from existing template file" $ do
      let mock = write dstDir fileDraft
      let mockLogs = Mock.getMockLogs mock mockConfig
      Mock.compileAndRenderTemplate_calls mockLogs
        `shouldBe` [(templatePath, templateData)]
      Mock.createDirectoryIfMissing_calls mockLogs
        `shouldBe` [(True, SP.toFilePath $ SP.parent expectedDstPath)]
      Mock.writeFileFromText_calls mockLogs
        `shouldBe` [(SP.toFilePath expectedDstPath, mockTemplateContent)]
  where
    (dstDir, dstPath, templatePath) =
      ( SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|a/b|],
        SP.fromPathRelFile [P.relfile|c/d/dst.txt|],
        SP.fromPathRelFile [P.relfile|e/tmpl.txt|]
      )
    templateData = object ["foo" .= ("bar" :: String)]
    fileDraft = createTemplateFileDraft dstPath templatePath (Just templateData)
    expectedDstPath = dstDir SP.</> dstPath
    mockTemplatesDirAbsPath = SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|mock/templates/dir|]
    mockTemplateContent = "Mock template content" :: Text
    mockConfig =
      Mock.defaultMockConfig
        { Mock.getTemplatesDirAbsPath_impl = mockTemplatesDirAbsPath,
          Mock.compileAndRenderTemplate_impl = \_ _ -> mockTemplateContent
        }
