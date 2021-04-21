module Parser.PageTest where

import           Test.Tasty.Hspec

import           Data.Either      (isLeft)
import qualified Path.Posix       as PPosix

import           Parser.Common    (runWaspParser)
import           Parser.Page      (page)
import qualified StrongPath       as SP
import qualified Wasp.JsImport
import qualified Wasp.Page


spec_parsePage :: Spec
spec_parsePage =
    describe "Parsing page declaration" $ do
        let parsePage input = runWaspParser page input

        let expectedPageComponentImport = Wasp.JsImport.JsImport
                { Wasp.JsImport._defaultImport = Just "Main"
                , Wasp.JsImport._namedImports = []
                , Wasp.JsImport._from = (SP.fromPathRelFileP [PPosix.relfile|pages/Main|])
                }

        it "When given a valid page declaration, returns correct AST" $ do
            let testPageName = "Landing"

            parsePage (
                "page " ++ testPageName ++ " { " ++
                    "component: import Main from \"@ext/pages/Main\"" ++
                "}")
                `shouldBe` Right (Wasp.Page.Page
                    { Wasp.Page._name = testPageName
                    , Wasp.Page._component = expectedPageComponentImport
                    , Wasp.Page._authRequired = Nothing
                    })

        it "When given a valid page with authRequired property, returns correct AST" $ do
            let testPageName = "Landing"

            parsePage (
                "page " ++ testPageName ++ " { " ++
                    "component: import Main from \"@ext/pages/Main\"," ++
                    "authRequired: true" ++
                "}")
                `shouldBe` Right (Wasp.Page.Page
                    { Wasp.Page._name = testPageName
                    , Wasp.Page._component = expectedPageComponentImport
                    , Wasp.Page._authRequired = Just True
                    })

        it "When given page wasp declaration without 'page', should return Left" $ do
            isLeft (parsePage "Landing { component: import Main from \"@ext/pages/Main\" }") `shouldBe` True
