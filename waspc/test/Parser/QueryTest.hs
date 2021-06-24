module Parser.QueryTest where

import Data.Either (isLeft)
import Parser.Common (runWaspParser)
import Parser.Query (query)
import qualified Path.Posix as PPosix
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Wasp.JsImport
import qualified Wasp.Query
import Data.Char (toLower)

spec_parseQuery :: Spec
spec_parseQuery =
  describe "Parsing query declaration" $ do
    let parseQuery = runWaspParser query

    it "When given a valid query declaration, returns correct AST(without auth)" $ do
      let testQuery = genQueryAST Nothing
      let testQueryInput = genQueryInput Nothing
      parseQuery testQueryInput `shouldBe` Right testQuery
    it "When given query wasp declaration without 'fn' property, should return Left" $ do
      isLeft (parseQuery "query myQuery { }") `shouldBe` True
    it "When given a valid query declaration, returns correct AST(with auth=true)" $ do
      let testQuery = genQueryAST (Just True)
      let testQueryInput = genQueryInput (Just True)
      parseQuery testQueryInput `shouldBe` Right testQuery
    it "When given a valid query declaration, returns correct AST(with auth=false)" $ do
      let testQuery = genQueryAST (Just False)
      let testQueryInput = genQueryInput (Just False)
      parseQuery testQueryInput `shouldBe` Right testQuery
        where
          genQueryInput :: Maybe Bool -> String
          genQueryInput qApplyAuth = (
                "query " ++ testQueryName ++ " {\n"
                  ++ "  fn: import { "
                  ++ testQueryJsFunctionName
                  ++ " } from \"@ext/some/path\",\n"
                  ++ "  entities: [Task, Project]"
                  ++ authStr qApplyAuth
                  ++ "}"
                )
          authStr :: Maybe Bool -> String
          authStr (Just useAuth) = ",\n  auth: " ++ map toLower (show useAuth) ++ "\n"
          authStr _ = "\n"                
          genQueryAST :: Maybe Bool -> Wasp.Query.Query
          genQueryAST qApplyAuth = Wasp.Query.Query
              { Wasp.Query._name = testQueryName,
                Wasp.Query._jsFunction =
                  Wasp.JsImport.JsImport
                    { Wasp.JsImport._defaultImport = Nothing,
                      Wasp.JsImport._namedImports = [testQueryJsFunctionName],
                      Wasp.JsImport._from = testQueryJsFunctionFrom
                    },
                Wasp.Query._entities = Just ["Task", "Project"],
                Wasp.Query._auth = qApplyAuth
              }
          testQueryName = "myQuery"
          testQueryJsFunctionName = "myJsQuery"
          testQueryJsFunctionFrom = SP.fromPathRelFileP [PPosix.relfile|some/path|]

