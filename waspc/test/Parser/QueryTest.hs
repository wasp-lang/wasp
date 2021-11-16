module Parser.QueryTest where

import Data.Char (toLower)
import Data.Either (isLeft)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.Parser.Common (runWaspParser)
import Wasp.Parser.Query (query)
import qualified Wasp.Wasp.JsImport as Wasp.JsImport
import qualified Wasp.Wasp.Query as Wasp.Query

spec_parseQuery :: Spec
spec_parseQuery =
  describe "Parsing query declaration" $ do
    let parseQuery = runWaspParser query
    let testWhenAuth auth =
          it ("When given a valid query declaration, returns correct AST(query.auth = " ++ show auth ++ ")") $
            parseQuery (genQueryCode auth) `shouldBe` Right (genQueryAST auth)
    testWhenAuth (Just True)
    testWhenAuth (Just False)
    testWhenAuth Nothing
    it "When given query wasp declaration without 'fn' property, should return Left" $ do
      isLeft (parseQuery "query myQuery { }") `shouldBe` True
  where
    genQueryCode :: Maybe Bool -> String
    genQueryCode qApplyAuth =
      "query " ++ testQueryName ++ " {\n"
          ++ "  fn: import { "
          ++ testQueryJsFunctionName
          ++ " } from \"@ext/some/path\",\n"
          ++ "  entities: [Task, Project]"
          ++ authStr qApplyAuth
          ++ "}"
    genQueryAST :: Maybe Bool -> Wasp.Query.Query
    genQueryAST qApplyAuth =
      Wasp.Query.Query
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

    authStr :: Maybe Bool -> String
    authStr (Just useAuth) = ",\n  auth: " ++ map toLower (show useAuth) ++ "\n"
    authStr _ = "\n"
    testQueryName = "myQuery"
    testQueryJsFunctionName = "myJsQuery"
    testQueryJsFunctionFrom = [SP.relfileP|some/path|]
