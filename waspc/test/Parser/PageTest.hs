module Parser.PageTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)

import StrongPath (Path, Rel, File)
import qualified StrongPath as SP
import qualified Path as P
import ExternalCode (SourceExternalCodeDir)

import Parser.Common (runWaspParser)
import Parser.Page (page)
import qualified Wasp.JsImport
import qualified Wasp.Page

spec_parsePage :: Spec
spec_parsePage =
    describe "Parsing page declaration" $ do
        let parsePage input = runWaspParser page input

        it "When given a valid page declaration, returns correct AST" $ do
            let testPageName = "Landing"

            let expectedPageComponentImport = Wasp.JsImport.JsImport
                    { Wasp.JsImport._defaultImport = Just "Main"
                    , Wasp.JsImport._namedImports = []
                    , Wasp.JsImport._from = (SP.fromPathRelFile [P.relfile|pages/Main|])
                        :: Path (Rel SourceExternalCodeDir) File
                    }

            parsePage (
                "page " ++ testPageName ++ " { " ++
                    "component: import Main from \"@ext/pages/Main\"" ++
                "}")
                `shouldBe` Right (Wasp.Page.Page
                    { Wasp.Page._name = testPageName
                    , Wasp.Page._component = expectedPageComponentImport
                    })

        it "When given page wasp declaration without 'page', should return Left" $ do
            isLeft (parsePage "Landing { component: import Main from \"@ext/pages/Main\" }") `shouldBe` True
