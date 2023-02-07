module JsImportTest where

import StrongPath (Dir', File', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.JsImport

spec_JsImportTest :: Spec
spec_JsImportTest = do
  describe "makeJsImport" $ do
    it "makes JsImport with default import from a path" $ do
      makeJsImport testJsFileImportPath (JsImportModule "test")
        `shouldBe` JsImport
          { _path = testJsFileImportPath,
            _name = JsImportModule "test",
            _importAlias = Nothing
          }
  describe "applyJsImportAlias" $ do
    it "applies alias to JsImport" $ do
      let jsImport = makeJsImport testJsFileImportPath (JsImportModule "test")
      applyJsImportAlias (Just "alias") jsImport
        `shouldBe` jsImport {_importAlias = Just "alias"}
  describe "getJsImportStmtAndIdentifier" $ do
    describe "generates import statement and identifier from" $ do
      it "default import" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testJsFileImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForTestJsFile ++ "'", "test")
      it "default import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeJsImport testJsFileImportPath (JsImportModule "test"))
          )
          `shouldBe` ("import alias from '" ++ generatedImportPathForTestJsFile ++ "'", "alias")
      it "named import" $ do
        getJsImportStmtAndIdentifier
          (makeJsImport testJsFileImportPath (JsImportField "test"))
          `shouldBe` ("import { test } from '" ++ generatedImportPathForTestJsFile ++ "'", "test")
      it "named import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeJsImport testJsFileImportPath (JsImportField "test"))
          )
          `shouldBe` ("import { test as alias } from '" ++ generatedImportPathForTestJsFile ++ "'", "alias")
  where
    testJsFileImportPath :: Path Posix (Rel Dir') File'
    testJsFileImportPath = [SP.reldirP|ext-src|] </> [SP.relfileP|folder/test.js|]

    generatedImportPathForTestJsFile = "./ext-src/folder/test.js"
