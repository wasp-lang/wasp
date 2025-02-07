module JsImportTest where

import StrongPath ((</>))
import qualified StrongPath as SP
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.JsImport

spec_JsImportTest :: Spec
spec_JsImportTest = do
  describe "makeJsImport" $ do
    it "makes JsImport with a module import from a path" $ do
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
      it "module import" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testRelativeImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "module import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeJsImport testRelativeImportPath (JsImportModule "test"))
          )
          `shouldBe` ("import alias from '" ++ generatedImportPathForRelativeImportPath ++ "'", "alias")
      it "module import with homonymous alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "test")
              (makeJsImport testRelativeImportPath (JsImportModule "test"))
          )
          `shouldBe` ("import test from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
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

      it "named import with homonymous alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "test")
              (makeJsImport testRelativeImportPath (JsImportField "test"))
          )
          `shouldBe` ("import { test } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "import from module" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testModuleImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForModuleImportPath ++ "'", "test")
  where
    testRelativeImportPath :: JsImportPath
    testRelativeImportPath = RelativeImportPath $ [SP.reldirP|src|] </> [SP.relfileP|folder/test.js|]

    generatedImportPathForRelativeImportPath = "./src/folder/test.js"

    testModuleImportPath :: JsImportPath
    testModuleImportPath = ModuleImportPath [SP.relfileP|wasp/server/api|]

    generatedImportPathForModuleImportPath :: String
    generatedImportPathForModuleImportPath = "wasp/server/api"
