module JsImportTest where

import StrongPath ((</>))
import qualified StrongPath as SP
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.JsImport

spec_JsImportTest :: Spec
spec_JsImportTest = do
  describe "makeValueJsImport" $ do
    it "makes a value JsImport from a path and a name" $ do
      makeValueJsImport testRelativeImportPath (JsImportModule "test")
        `shouldBe` JsImport
          { _kind = ValueImport,
            _path = testRelativeImportPath,
            _name = JsImportModule "test",
            _importAlias = Nothing
          }
  describe "makeTypeJsImport" $ do
    it "makes a type JsImport from a path and a name" $ do
      makeTypeJsImport testRelativeImportPath (JsImportField "test")
        `shouldBe` JsImport
          { _kind = TypeImport,
            _path = testRelativeImportPath,
            _name = JsImportField "test",
            _importAlias = Nothing
          }
  describe "applyJsImportAlias" $ do
    it "applies alias to JsImport" $ do
      let jsImport = makeValueJsImport testRelativeImportPath (JsImportModule "test")
      applyJsImportAlias (Just "alias") jsImport
        `shouldBe` jsImport {_importAlias = Just "alias"}
  describe "getJsImportStmtAndIdentifier" $ do
    describe "generates import statement and identifier from" $ do
      it "module import" $ do
        getJsImportStmtAndIdentifier
          (makeValueJsImport testRelativeImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "module import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeValueJsImport testRelativeImportPath (JsImportModule "test"))
          )
          `shouldBe` ("import alias from '" ++ generatedImportPathForRelativeImportPath ++ "'", "alias")
      it "module import with homonymous alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "test")
              (makeValueJsImport testRelativeImportPath (JsImportModule "test"))
          )
          `shouldBe` ("import test from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "named import" $ do
        getJsImportStmtAndIdentifier
          (makeValueJsImport testRelativeImportPath (JsImportField "test"))
          `shouldBe` ("import { test } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "named import with alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "alias")
              (makeValueJsImport testRelativeImportPath (JsImportField "test"))
          )
          `shouldBe` ("import { test as alias } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "alias")

      it "named import with homonymous alias" $ do
        getJsImportStmtAndIdentifier
          ( applyJsImportAlias
              (Just "test")
              (makeValueJsImport testRelativeImportPath (JsImportField "test"))
          )
          `shouldBe` ("import { test } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
      it "import from module" $ do
        getJsImportStmtAndIdentifier
          (makeValueJsImport testModuleImportPath (JsImportModule "test"))
          `shouldBe` ("import test from '" ++ generatedImportPathForModuleImportPath ++ "'", "test")
      it "type import" $ do
        getJsImportStmtAndIdentifier
          (makeTypeJsImport testRelativeImportPath (JsImportField "test"))
          `shouldBe` ("import type { test } from '" ++ generatedImportPathForRelativeImportPath ++ "'", "test")
  describe "getJsDynamicImportExpression" $ do
    it "generates value (runtime) dynamic import expression for default export" $ do
      getJsDynamicImportExpression (makeValueJsImport testRelativeImportPath (JsImportModule "test"))
        `shouldBe` "import('" ++ generatedImportPathForRelativeImportPath ++ "').then(m => m.default)"
    it "generates value (runtime) dynamic import expression for named export" $ do
      getJsDynamicImportExpression (makeValueJsImport testRelativeImportPath (JsImportField "test"))
        `shouldBe` "import('" ++ generatedImportPathForRelativeImportPath ++ "').then(m => m.test)"
    it "generates type dynamic import expression for default export" $ do
      getJsDynamicImportExpression (makeTypeJsImport testRelativeImportPath (JsImportModule "test"))
        `shouldBe` "import('" ++ generatedImportPathForRelativeImportPath ++ "').default"
    it "generates type dynamic import expression for named export" $ do
      getJsDynamicImportExpression (makeTypeJsImport testRelativeImportPath (JsImportField "test"))
        `shouldBe` "import('" ++ generatedImportPathForRelativeImportPath ++ "').test"
  where
    testRelativeImportPath :: JsImportPath
    testRelativeImportPath = RelativeImportPath $ [SP.reldirP|src|] </> [SP.relfileP|folder/test.js|]

    generatedImportPathForRelativeImportPath = "./src/folder/test.js"

    testModuleImportPath :: JsImportPath
    testModuleImportPath = ModuleImportPath [SP.relfileP|wasp/server/api|]

    generatedImportPathForModuleImportPath :: String
    generatedImportPathForModuleImportPath = "wasp/server/api"
