module Parser.ParserTest where

import Test.Tasty.Hspec
import Data.Either
import Path (relfile)

import Parser
import Wasp
import qualified Wasp.EntityForm as EF
import qualified Wasp.EntityList as EL
import qualified Wasp.Style
import qualified Wasp.JsCode


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
                        , pageStyle = Just $ Wasp.Style.CssCode "div {\n\
                                      \          color: red\n\
                                      \        }"
                        }
                    , WaspElementPage $ Page
                        { pageName = "TestPage"
                        , pageRoute = "/test"
                        , pageContent = "<div>This is a test page!</div>"
                        , pageStyle = Just $ Wasp.Style.ExtCodeCssFile [relfile|test.css|]
                        }
                    , WaspElementEntity $ Entity
                        { entityName = "Task"
                        , entityFields =
                            [ Wasp.EntityField "description" Wasp.EftString
                            , Wasp.EntityField "isDone" Wasp.EftBoolean
                            ]
                        }
                    , WaspElementEntityForm $ EF.EntityForm
                        { EF._name = "CreateTaskForm"
                        , EF._entityName = "Task"
                        , EF._submit = Just EF.Submit
                            { EF._onEnter = Just False
                            , EF._submitButton = Just EF.SubmitButton
                                { EF._submitButtonShow = Just True
                                }
                            }
                        , EF._fields =
                            [ EF.Field
                                { EF._fieldName = "description"
                                , EF._fieldShow = Just True
                                , EF._fieldDefaultValue = Just $ EF.DefaultValueString "doable task"
                                , EF._fieldPlaceholder = Just "What will you do?"
                                }
                            , EF.Field
                                { EF._fieldName = "isDone"
                                , EF._fieldShow = Just False
                                , EF._fieldDefaultValue = Just $ EF.DefaultValueBool False
                                , EF._fieldPlaceholder = Nothing
                                }
                            ]
                        }
                    , WaspElementEntityList $ EL.EntityList
                        { EL._name = "TaskList"
                        , EL._entityName = "Task"
                        , EL._showHeader = Just False
                        , EL._fields =
                            [ EL.Field 
                                { EL._fieldName = "description"
                                , EL._fieldRender = Just $ Wasp.JsCode.JsCode "task => task.description"
                                }
                            ]
                        }
                    ]
                    `setJsImports` [ JsImport "something" [relfile|some/file|] ]
                )
