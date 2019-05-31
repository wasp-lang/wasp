module Parser.ParserTest where

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
            it "When given a valid wasp source, should return correct\
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
                        , pageRoute = "/"
                        , pageContent = "<div>My landing page! I have { this.props.taskList.length } tasks. </div>"
                        }
                    , WaspElementPage $ Page
                        { pageName = "TestPage"
                        , pageRoute = "/test"
                        , pageContent = "<div>This is a test page!</div>"
                        }
                    , WaspElementEntity $ Entity
                        { entityName = "Task"
                        , entityFields =
                            [ Wasp.EntityField "description" Wasp.EftString
                            , Wasp.EntityField "isDone" Wasp.EftBoolean
                            ]
                        }
                    ]
                )
