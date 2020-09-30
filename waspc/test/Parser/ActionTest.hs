module Parser.ActionTest where

import           Test.Tasty.Hspec

import           Data.Either      (isLeft)
import qualified Path.Posix       as PPosix

import           Parser.Action    (action)
import           Parser.Common    (runWaspParser)
import qualified StrongPath       as SP
import qualified Wasp.Action
import qualified Wasp.JsImport

-- TODO: This file is mostly just duplication of Parser.QueryTest.
--   We might want to look into generalizing the two, to avoid all this
--   duplication -> well, we really shoud start with generalizing
--   Parser.Query and Parser.Action.

spec_parseAction :: Spec
spec_parseAction =
    describe "Parsing action declaration" $ do
        let parseAction = runWaspParser action

        it "When given a valid action declaration, returns correct AST" $ do
            let testActionName = "myAction"
                testActionJsFunctionName = "myJsAction"
                testActionJsFunctionFrom = SP.fromPathRelFileP [PPosix.relfile|some/path|]
            let testAction = Wasp.Action.Action
                    { Wasp.Action._name = testActionName
                    , Wasp.Action._jsFunction = Wasp.JsImport.JsImport
                        { Wasp.JsImport._defaultImport = Nothing
                        , Wasp.JsImport._namedImports = [ testActionJsFunctionName ]
                        , Wasp.JsImport._from = testActionJsFunctionFrom
                        }
                    , Wasp.Action._entities = Nothing
                    }
            parseAction ( "action " ++ testActionName ++ " {\n" ++
                         "  fn: import { " ++ testActionJsFunctionName ++ " } from \"@ext/some/path\"\n" ++
                         "}"
                       ) `shouldBe` Right testAction
        it "When given action wasp declaration without 'fn' property, should return Left" $ do
            isLeft (parseAction "action myAction { }") `shouldBe` True
