module Generator.ExternalCodeGenerator.JsTest where

import Generator.ExternalCodeGenerator.Common (asGenExtFile)
import Generator.ExternalCodeGenerator.Js as Js
import qualified StrongPath as SP
import Test.Tasty.Hspec

spec_resolveJsFileWaspImportsForExtCodeDir :: Spec
spec_resolveJsFileWaspImportsForExtCodeDir = do
  (asGenExtFile [SP.relfile|extFile.js|], "import foo from 'bar'") ~> "import foo from 'bar'"
  (asGenExtFile [SP.relfile|extFile.js|], "import foo from '@wasp/bar'") ~> "import foo from '../bar'"
  (asGenExtFile [SP.relfile|a/extFile.js|], "import foo from  \"@wasp/bar/foo\"")
    ~> "import foo from  \"../../bar/foo\""
  where
    (path, text) ~> expectedText =
      it (SP.toFilePath path ++ " " ++ show text ++ " -> " ++ show expectedText) $ do
        Js.resolveJsFileWaspImportsForExtCodeDir [SP.reldir|src|] path text `shouldBe` expectedText
