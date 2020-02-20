module Generator.ExternalCode.JsTest where

import Test.Tasty.Hspec

import Path (relfile)

import Generator.ExternalCode.Js


spec_resolveJsFileWaspImports :: Spec
spec_resolveJsFileWaspImports = do
    ([relfile|ext-src/extFile.js|], "import foo from 'bar'") ~> "import foo from 'bar'"
    ([relfile|ext-src/extFile.js|], "import foo from '@wasp/bar'") ~> "import foo from '../bar'"
    ([relfile|ext-src/a/extFile.js|], "import foo from  \"@wasp/bar/foo\"") ~>
        "import foo from  \"../../bar/foo\""
  where
    (path, text) ~> expectedText =
        it ((show path) ++ " " ++ (show text) ++ " -> " ++ (show expectedText)) $ do
            resolveJsFileWaspImports path text `shouldBe` expectedText
