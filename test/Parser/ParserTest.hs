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
                        -- TODO: This is heavily hardcoded and hard to maintain, we should find
                        --   better way to test this (test a property, not exact text?) Or keep valid.wasp simple?
                        --   Or use manual snapshot file as Matija suggested?
                        , pageContent = "<div>\n\
                                        \          My landing page! I have { this.props.taskList.length } tasks.\n\
                                        \\n\
                                        \          <div>\n\
                                        \            <TaskCreateForm\n\
                                        \              onCreate={task => this.props.addTask(task)}\n\
                                        \              submitButtonLabel={'Create new task'}\n\
                                        \            />\n\
                                        \          </div>\n\
                                        \\n\
                                        \          My tasks\n\
                                        \          <TaskList />\n\
                                        \        </div>"
                        , pageStyle = Just "div {\n\
                                      \          color: red\n\
                                      \        }"
                        }
                    , WaspElementPage $ Page
                        { pageName = "TestPage"
                        , pageRoute = "/test"
                        , pageContent = "<div>This is a test page!</div>"
                        , pageStyle = Nothing
                        }
                    , WaspElementEntity $ Entity
                        { entityName = "Task"
                        , entityFields =
                            [ Wasp.EntityField "description" Wasp.EftString
                            , Wasp.EntityField "isDone" Wasp.EftBoolean
                            ]
                        }
                    , WaspElementEntityForm $ EntityForm
                        { efName = "CreateTaskForm"
                        , efEntityName = "Task"
                        , efSubmitConfig = Just EntityFormSubmitConfig
                            { onEnter = False
                            }
                        }
                    ]
                )
