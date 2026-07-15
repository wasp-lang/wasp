module Generator.JsImportTest where

import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import Test.Hspec
import Wasp.AppSpec.ExtImport
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import Wasp.Generator.JsImport
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import Wasp.JsImport as JI

spec_GeneratorJsImportTest :: Spec
spec_GeneratorJsImportTest = do
  describe "extImportToJsImport" $ do
    let pathToExtCodeDir = [SP.reldirP|src|] :: (Path Posix (Rel ServerSrcDir) (Dir GeneratedExternalCodeDir))
        pathFromImportLocationToExtCodeDir = [SP.reldirP|../|]
        extImport =
          ExtImport
            { name = ExtImportModule "test",
              path = [SP.relfileP|folder/test.js|],
              alias = Nothing
            }
    it "makes a JsImport from ExtImport" $ do
      extImportToJsImport pathToExtCodeDir pathFromImportLocationToExtCodeDir extImport
        `shouldBe` JI.JsImport
          { JI._kind = JI.ValueImport,
            JI._path = JI.RelativeImportPath [SP.relfileP|../src/folder/test.js|],
            JI._name = JsImportModule "test",
            JI._importAlias = Nothing
          }
    it "uses alias metadata for generated ExtImport identifiers" $ do
      getAliasedExtImportIdentifier extImport {alias = Just "testAlias"}
        `shouldBe` "testAlias_ext"
    it "avoids collisions for same exported name with different aliases" $ do
      let firstImport = extImport {name = ExtImportField "handler", path = [SP.relfileP|one.js|], alias = Just "oneHandler"}
          secondImport = extImport {name = ExtImportField "handler", path = [SP.relfileP|two.js|], alias = Just "twoHandler"}
      getAliasedExtImportIdentifier <$> [firstImport, secondImport]
        `shouldBe` ["oneHandler_ext", "twoHandler_ext"]
