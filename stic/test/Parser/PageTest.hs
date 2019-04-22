module Parser.PageTest where

import Test.Tasty.Hspec

import Data.Either
import Text.Parsec

import Parser.Page
import Wasp

-- | Page parser executor.
parsePage :: String -> Either ParseError Wasp.Page
parsePage pageWasp = parse page "" pageWasp

spec_parsePage :: Spec
spec_parsePage =
    describe "Parsing page wasp" $ do
        it "When given valid page wasp declaration, returns correct Wasp.Page" $ do
            let testPageName = "Landing"
            let testPageRoute = "/someRoute"
            let testPageContent = "<span/>"
            parsePage (
                "page " ++ testPageName ++ " { " ++
                    "route: \"" ++ testPageRoute ++ "\"," ++ 
                    "content: { " ++ testPageContent ++ " }" ++ 
                "}")
                `shouldBe` Right (Page
                    { pageName = testPageName
                    , pageRoute = testPageRoute
                    , pageContent = testPageContent
                    })

        it "When given page wasp declaration without 'page', should return Left" $ do
            isLeft (parsePage "Landing { route: someRoute })") `shouldBe` True
