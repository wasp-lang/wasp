module Parser.PageTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)

import Parser.Common (runWaspParser)
import Parser.Page (page)
import qualified Wasp

spec_parsePage :: Spec
spec_parsePage =
    describe "Parsing page declaration" $ do
        let parsePage input = runWaspParser page input

        it "When given a valid page declaration, returns correct AST" $ do
            let testPageName = "Landing"
            let testPageContent = "<span/>"
            parsePage (
                "page " ++ testPageName ++ " { " ++
                    "content: {=jsx " ++ testPageContent ++ " jsx=}" ++
                "}")
                `shouldBe` Right (Wasp.Page
                    { Wasp.pageName = testPageName
                    , Wasp.pageContent = testPageContent
                    , Wasp.pageStyle = Nothing
                    })

        it "When given page wasp declaration without 'page', should return Left" $ do
            isLeft (parsePage "Landing { route: someRoute, content: <span/> }") `shouldBe` True
