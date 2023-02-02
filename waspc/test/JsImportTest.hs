module JsImportTest where

import StrongPath (Dir', File', Path, Posix, Rel, (</>))
import qualified StrongPath as SP
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.JsImport

spec_JsImportTest :: Spec
spec_JsImportTest = do
  describe "makeJsImport" $ do
    it "makes JsImport with default import from a path" $ do
      makeJsImport pathToJsFile (JsImportModule "test")
        `shouldBe` JsImport
          { _path = pathToJsFile,
            _name = JsImportModule "test",
            _importAlias = Nothing
          }
  describe "applyJsImportAlias" $ do
    it "applies alias to JsImport" $ do
      applyJsImportAlias
        (Just "alias")
        (makeJsImport pathToJsFile (JsImportModule "test"))
        `shouldBe` JsImport
          { _path = pathToJsFile,
            _name = JsImportModule "test",
            _importAlias = Just "alias"
          }
  describe "generateJsImportStatement" $ do
    it "generates import statement for default import" $ do
      getJsImportStmtAndIdentifier
        (makeJsImport pathToJsFile (JsImportModule "test"))
        `shouldBe` ("import test from './ext-src/folder/test.js'", "test")
    it "generates import statement for default import with alias" $ do
      getJsImportStmtAndIdentifier
        ( applyJsImportAlias
            (Just "alias")
            (makeJsImport pathToJsFile (JsImportModule "test"))
        )
        `shouldBe` ("import alias from './ext-src/folder/test.js'", "alias")
    it "generates import statement for named import" $ do
      getJsImportStmtAndIdentifier
        (makeJsImport pathToJsFile (JsImportField "test"))
        `shouldBe` ("import { test } from './ext-src/folder/test.js'", "test")
    it "generates import statement for named import with alias" $ do
      getJsImportStmtAndIdentifier
        ( applyJsImportAlias
            (Just "alias")
            (makeJsImport pathToJsFile (JsImportField "test"))
        )
        `shouldBe` ("import { test as alias } from './ext-src/folder/test.js'", "alias")
  where
    pathToJsFile :: Path Posix (Rel Dir') File'
    pathToJsFile = [SP.reldirP|ext-src|] </> [SP.relfileP|folder/test.js|]
