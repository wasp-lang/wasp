module Parser.OperationTest where

import Data.List (intercalate)
import Parser.Common (runWaspParser)
import Parser.Operation
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Wasp.JsImport

spec_parseOperation :: Spec
spec_parseOperation =
  describe "Parsing operation properties" $ do
    let parseOperationProperties = runWaspParser properties

    it "When given a valid list of properties, correctly parses them" $ do
      let testJsFnName = "myJsFn"
          testJsFnFrom = [SP.relfileP|some/path|]
      let testProps =
            [ JsFunction $
                Wasp.JsImport.JsImport
                  { Wasp.JsImport._defaultImport = Nothing,
                    Wasp.JsImport._namedImports = [testJsFnName],
                    Wasp.JsImport._from = testJsFnFrom
                  },
              Entities ["Task", "Project"]
            ]
      parseOperationProperties
        ( intercalate
            ",\n"
            [ "fn: import { " ++ testJsFnName ++ " } from \"@ext/some/path\"",
              "entities: [Task, Project]"
            ]
        )
        `shouldBe` Right testProps
