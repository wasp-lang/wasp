module Parser.PageTest where

import Test.Tasty.Hspec

import Data.Either (isLeft)

import Parser.Common (runWaspParser)
import Parser.Page (page)
import qualified Wasp

spec_parsePage :: Spec
spec_parsePage =
    describe "Parsing page wasp" $ do
        let parsePage input = runWaspParser page input

        it "When given valid page wasp declaration, returns correct Wasp.Page" $ do
            let testPageName = "Landing"
            let testPageRoute = "/someRoute"
            let testPageContent = "<span/>"
            parsePage (
                "page " ++ testPageName ++ " { " ++
                    "route: \"" ++ testPageRoute ++ "\"," ++ 
                    "content: {=jsx " ++ testPageContent ++ " jsx=}" ++ 
                "}")
                `shouldBe` Right (Wasp.Page
                    { Wasp.pageName = testPageName
                    , Wasp.pageRoute = testPageRoute
                    , Wasp.pageContent = testPageContent
                    })

        it "When given page wasp declaration without 'page', should return Left" $ do
            isLeft (parsePage "Landing { route: someRoute }") `shouldBe` True
