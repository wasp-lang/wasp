module Generator.JsImportTest where

import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.AppSpec.ExtImport
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.JsImport
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import Wasp.JsImport as JI

spec_GeneratorJsImportTest :: Spec
spec_GeneratorJsImportTest = do
  describe "extImportToJsImport" $ do
    let pathToExtCodeDir = [SP.reldirP|ext-src|] :: (Path Posix (Rel ServerSrcDir) (Dir GeneratedExternalCodeDir))
        pathFromImportLocationToExtCodeDir = [SP.reldirP|../|]
        extImport =
          ExtImport
            { name = ExtImportModule "test",
              path = [SP.relfileP|folder/test.js|]
            }
    it "makes a JsImport from ExtImport" $ do
      extImportToJsImport pathToExtCodeDir pathFromImportLocationToExtCodeDir extImport
        `shouldBe` JI.JsImport
          { JI._path = [SP.relfileP|../ext-src/folder/test.js|],
            JI._name = JsImportModule "test",
            JI._importAlias = Nothing
          }
