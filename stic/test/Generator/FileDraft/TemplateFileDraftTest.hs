{-# LANGUAGE OverloadedStrings #-}
module Generator.FileDraft.TemplateFileDraftTest where

import qualified Test.Tasty
import Test.Tasty.Hspec

import Data.Aeson (object, (.=))
import System.FilePath (FilePath, (</>), takeDirectory)
import Data.Text (Text, pack)

import Generator.FileDraft

import Generator.MockFileDraftIO


spec_TemplateFileDraft :: Spec
spec_TemplateFileDraft = do
    describe "writeToFile" $ do
        it "Creates new file from existing template file" $ do
            let mock = writeToFile dstDir fileDraft
            let mockLogs = getMockLogs mock mockConfig
            compileAndRenderTemplate_calls mockLogs
                `shouldBe` [(templatePath, templateData)]
            createDirectoryIfMissing_calls mockLogs
                `shouldBe` [(True, takeDirectory expectedDstPath)]
            writeFileFromText_calls mockLogs
                `shouldBe` [(expectedDstPath, mockTemplateContent)]
              where
                (dstDir, dstPath, templatePath) = ("a/b", "c/d/dst.txt", "e/tmpl.txt")
                templateData = object [ "foo" .= ("bar" :: String) ]
                fileDraft = createTemplateFileDraft dstPath templatePath templateData
                expectedTemplatePath = mockTemplatesDirAbsPath </> templatePath
                expectedDstPath = dstDir </> dstPath
                mockTemplatesDirAbsPath = "mock/templates/dir"
                mockTemplateContent = "Mock template content" :: Text
                mockConfig = defaultMockConfig
                    { getTemplatesDirAbsPath_impl = mockTemplatesDirAbsPath
                    , compileAndRenderTemplate_impl = \_ _ -> mockTemplateContent
                    }
