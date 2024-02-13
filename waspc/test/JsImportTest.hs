module JsImportTest where

import StrongPath ((</>))
import qualified StrongPath as SP
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.JsImport

spec_JsImportTest :: Spec
spec_JsImportTest = do
  describe "makeJsImport" $ do
    it "makes JsImport with default import from a path" $ do
      makeJsImport testRelativeImportPath (JsImportModule "test")
        `shouldBe` JsImport
          { _path = testRelativeImportPath,
            _name = JsImportModule "test",
            _importAlias = Nothing
          }
  describe "applyJsImportAlias" $ do
    it "applies alias to JsImport" $ do
      let jsImport = makeJsImport testRelativeImportPath (JsImportModule "test")
      applyJsImportAlias (Just "alias") jsImport
        `shouldBe` jsImport {_importAlias = Just "alias"}
  describe "getJsImportStmtAndIdentifier" $ do
    describe "generates import statement and identifier from" $ do
      it "default import" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testRelativeImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "default import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeJsImport testRelativeImportPath (JsImportModule "test"))
          )
          `shouldBe` ("import alias from '" ++ generatedImportPathForRelativeImportPath ++ "'", "alias")
      it "named import" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testRelativeImportPath (JsImportField "test"))
          `shouldBe` ("import { test } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "named import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeJsImport testRelativeImportPath (JsImportField "test"))
          )
          `shouldBe` ("import { test as alias } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "alias")
      it "import from module" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testModuleImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForModuleImportPath ++ "'", "test")
  where
    testRelativeImportPath :: JsImportPath
    testRelativeImportPath = RelativeImportPath $ [SP.reldirP|ext-src|] </> [SP.relfileP|folder/test.js|]

    generatedImportPathForRelativeImportPath = "./ext-src/folder/test.js"

    testModuleImportPath :: JsImportPath
    testModuleImportPath = ModuleImportPath [SP.relfileP|wasp/server/api|]

    generatedImportPathForModuleImportPath :: String
    generatedImportPathForModuleImportPath = "wasp/server/api"
