module Parser.ParserTest where

import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.Either

import Parser
import Wasp

spec_parseWasp :: Spec
spec_parseWasp = 
    describe "Parsing wasp" $ do
        it "When given wasp without app, should return Left" $ do
            isLeft (parseWasp "hoho") `shouldBe` True

        before (readFile "test/Parser/valid.wasp") $ do
            it "When given a valid wasp with app and name, should return correct\
                \ Wasp" $ \wasp -> do
                parseWasp wasp
                `shouldBe`
                Right (fromWaspElems
                    [ WaspElementApp $ App
                        { appName = "test_app"
                        , appTitle = "Hello World!"
                        }
                    , WaspElementPage $ Page
                        { pageName = "Landing"
                        , pageRoute = "/home"
                        , pageContent = "<div>My landing page!</div>"
                        }
                    ]
                )
