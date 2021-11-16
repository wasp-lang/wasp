module Parser.ActionTest where

import Data.Char (toLower)
import Data.Either (isLeft)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.Parser.Action (action)
import Wasp.Parser.Common (runWaspParser)
import qualified Wasp.Wasp.Action as Wasp.Action
import qualified Wasp.Wasp.JsImport as Wasp.JsImport

-- TODO: This file is mostly just duplication of Parser.QueryTest.
--   We might want to look into generalizing the two, to avoid all this
--   duplication -> well, we really shoud start with generalizing
--   Parser.Query and Parser.Action.

spec_parseAction :: Spec
spec_parseAction =
  describe "Parsing action declaration" $ do
    let parseAction = runWaspParser action
    let testWhenAuth auth =
          it ("When given a valid action declaration, returns correct AST(action.auth = " ++ show auth ++ ")") $
            parseAction (genActionCode auth) `shouldBe` Right (genActionAST auth)
    testWhenAuth (Just True)
    testWhenAuth (Just False)
    testWhenAuth Nothing
    it "When given action wasp declaration without 'fn' property, should return Left" $ do
      isLeft (parseAction "action myAction { }") `shouldBe` True
  where
    genActionAST :: Maybe Bool -> Wasp.Action.Action
    genActionAST aApplyAuth =
      Wasp.Action.Action
        { Wasp.Action._name = testActionName,
          Wasp.Action._jsFunction =
            Wasp.JsImport.JsImport
              { Wasp.JsImport._defaultImport = Nothing,
                Wasp.JsImport._namedImports = [testActionJsFunctionName],
                Wasp.JsImport._from = testActionJsFunctionFrom
              },
          Wasp.Action._entities = Nothing,
          Wasp.Action._auth = aApplyAuth
        }
    genActionCode :: Maybe Bool -> String
    genActionCode aApplyAuth =
      "action " ++ testActionName ++ " {\n"
        ++ "  fn: import { "
        ++ testActionJsFunctionName
        ++ " } from \"@ext/some/path\""
        ++ authStr aApplyAuth
        ++ "}"

    authStr :: Maybe Bool -> String
    authStr (Just useAuth) = ",\n  auth: " ++ map toLower (show useAuth) ++ "\n"
    authStr _ = "\n"
    testActionJsFunctionFrom = [SP.relfileP|some/path|]
    testActionJsFunctionName = "myJsAction"
    testActionName = "myAction"
