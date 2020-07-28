module Generator.WebAppGenerator.ExternalCodeGeneratorTest where

import Test.Tasty.Hspec
import qualified Path as P

import qualified StrongPath as SP
import Generator.WebAppGenerator.ExternalCodeGenerator as G
import Generator.ExternalCodeGenerator.Common (asGenExtFile)

spec_resolveJsFileWaspImports :: Spec
spec_resolveJsFileWaspImports = do
    (asGenExtFile [P.relfile|extFile.js|], "import foo from 'bar'") ~> "import foo from 'bar'"
    (asGenExtFile [P.relfile|extFile.js|], "import foo from '@wasp/bar'") ~> "import foo from '../bar'"
    (asGenExtFile [P.relfile|a/extFile.js|], "import foo from  \"@wasp/bar/foo\"") ~>
        "import foo from  \"../../bar/foo\""
  where
    (path, text) ~> expectedText =
        it ((SP.toFilePath path) ++ " " ++ (show text) ++ " -> " ++ (show expectedText)) $ do
        G.resolveJsFileWaspImports path text `shouldBe` expectedText

