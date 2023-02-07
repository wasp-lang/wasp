module Generator.JsImportTest where

import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.AppSpec.ExtImport
import Wasp.Generator.JsImport
import Wasp.JsImport as JI

spec_GeneratorJsImportTest :: Spec
spec_GeneratorJsImportTest = do
  describe "extImportToJsImport" $ do
    let pathToExtCodeDir = [SP.reldirP|ext-src|]
        pathFromImportLocationToExtCodeDir = [SP.reldirP|../|]
        extImport =
          ExtImport
            { name = ExtImportModule "test",
              path = [SP.relfileP|folder/test.js|]
            }
    it "makes a JsImport from path to ext code dir, relative dir to ext code dir and ExtImport" $ do
      extImportToJsImport pathToExtCodeDir pathFromImportLocationToExtCodeDir extImport
        `shouldBe` JI.JsImport
          { JI._path = [SP.relfileP|../ext-src/folder/test.js|],
            JI._name = JsImportModule "test",
            JI._importAlias = Nothing
          }
