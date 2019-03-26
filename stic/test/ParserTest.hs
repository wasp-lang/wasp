module ParserTest where

import qualified Test.Tasty
import Test.Tasty.Hspec
import Data.Either

import Parser
import Wasp

createWaspWithApp :: String -> Wasp
createWaspWithApp parsedAppName = fromApp $
    App { appName=parsedAppName, appTitle="someTitle" }

spec_parseWasp :: Spec
spec_parseWasp = do
    describe "Parsing wasp" $ do
        it "When given wasp without app, should return Left" $ do
            isLeft (parseWasp "hoho") `shouldBe` True

        it "When given wasp with app and name, should return correct Wasp" $ do
            parseWasp "app testApp"
            `shouldBe`
            Right (createWaspWithApp "testApp")

            
