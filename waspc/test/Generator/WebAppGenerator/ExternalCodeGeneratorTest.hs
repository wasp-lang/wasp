module Generator.WebAppGenerator.ExternalCodeGeneratorTest where

import Test.Tasty.Hspec

import Path (relfile)

import Generator.WebAppGenerator.ExternalCodeGenerator as G

spec_resolveJsFileWaspImports :: Spec
spec_resolveJsFileWaspImports = do
    ([relfile|extFile.js|], "import foo from 'bar'") ~> "import foo from 'bar'"
    ([relfile|extFile.js|], "import foo from '@wasp/bar'") ~> "import foo from '../bar'"
    ([relfile|a/extFile.js|], "import foo from  \"@wasp/bar/foo\"") ~>
        "import foo from  \"../../bar/foo\""
  where
    (path, text) ~> expectedText =
        it ((show path) ++ " " ++ (show text) ++ " -> " ++ (show expectedText)) $ do
        G.resolveJsFileWaspImports path text `shouldBe` expectedText
