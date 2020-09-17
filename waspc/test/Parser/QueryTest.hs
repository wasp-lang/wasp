module Parser.QueryTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)
import qualified Path.Posix as PPosix

import Parser.Common (runWaspParser)
import Parser.Query (query)
import qualified Wasp.Query
import qualified Wasp.JsImport

spec_parseQuery :: Spec
spec_parseQuery =
    describe "Parsing query declaration" $ do
        let parseQuery = runWaspParser query

        it "When given a valid query declaration, returns correct AST" $ do
            let testQueryName = "myQuery"
                testQueryJsFunctionName = "myJsQuery"
                testQueryJsFunctionFrom = [PPosix.relfile|some/path|]
            let testQuery = Wasp.Query.Query
                    { Wasp.Query._name = testQueryName
                    , Wasp.Query._jsFunction = Wasp.JsImport.JsImport
                        { Wasp.JsImport._defaultImport = Nothing
                        , Wasp.JsImport._namedImports = [ testQueryJsFunctionName ]
                        , Wasp.JsImport._from = testQueryJsFunctionFrom
                        }
                    }
            parseQuery ( "query " ++ testQueryName ++ " {\n" ++
                         "  fn: import { " ++ testQueryJsFunctionName ++ " } from \"@ext/some/path\"\n" ++
                         "}"
                       ) `shouldBe` Right testQuery
        it "When given query wasp declaration without 'fn' property, should return Left" $ do
            isLeft (parseQuery "query myQuery { }") `shouldBe` True
