module Generator.JsImportTest where

import StrongPath (Dir, Path, Posix, Rel)
import qualified StrongPath as SP
import Test.Hspec
import Wasp.AppSpec.ExtImport
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
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
              source = ProjectSrcExtImportSource [SP.relfileP|folder/test.js|],
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
      let firstImport = extImport {name = ExtImportField "handler", source = ProjectSrcExtImportSource [SP.relfileP|one.js|], alias = Just "oneHandler"}
          secondImport = extImport {name = ExtImportField "handler", source = ProjectSrcExtImportSource [SP.relfileP|two.js|], alias = Just "twoHandler"}
      getAliasedExtImportIdentifier <$> [firstImport, secondImport]
        `shouldBe` ["oneHandler_ext", "twoHandler_ext"]
    it "makes a raw JsImport from package ExtImport" $ do
      let packageExtImport =
            extImport
              { source = PackageExtImportSource $ PackageImportSource "@skateboard/fsm" (Just "SkateboardPage")
              }
      extImportToJsImport pathToExtCodeDir pathFromImportLocationToExtCodeDir packageExtImport
        `shouldBe` JI.JsImport
          { JI._kind = JI.ValueImport,
            JI._path = JI.RawImportName "@skateboard/fsm/SkateboardPage",
            JI._name = JsImportModule "test",
            JI._importAlias = Nothing
          }
