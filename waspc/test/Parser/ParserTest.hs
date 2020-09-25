module Parser.ParserTest where

import           Data.Either
import qualified Path.Posix           as PPosix
import           Test.Tasty.Hspec

import           NpmDependency        as ND
import           Parser
import qualified StrongPath           as SP
import           Wasp
import qualified Wasp.EntityPSL
import qualified Wasp.JsCode
import qualified Wasp.JsImport
import qualified Wasp.NpmDependencies
import qualified Wasp.Page
import qualified Wasp.Query
import qualified Wasp.Route           as R

-- TODO(matija): old Entity stuff, to be removed.
import qualified Wasp.EntityForm      as EF
import qualified Wasp.EntityList      as EL


spec_parseWasp :: Spec
spec_parseWasp =
    describe "Parsing wasp" $ do
        it "When given wasp without app, should return Left" $ do
            isLeft (parseWasp "hoho") `shouldBe` True

        before (readFile "test/Parser/valid.wasp") $ do
            it "When given a valid wasp source, should return correct Wasp" $ \wasp -> do
                parseWasp wasp
                `shouldBe`
                Right (fromWaspElems
                    [ WaspElementApp $ App
                        { appName = "test_app"
                        , appTitle = "Hello World!"
                        }
                    , WaspElementRoute $ R.Route
                        { R._urlPath = "/"
                        , R._targetPage = "Landing"
                        }
                    , WaspElementPage $ Wasp.Page.Page
                        { Wasp.Page._name = "Landing"
                        , Wasp.Page._component = Wasp.JsImport.JsImport
                            { Wasp.JsImport._defaultImport = Just "Landing"
                            , Wasp.JsImport._namedImports = []
                            , Wasp.JsImport._from = SP.fromPathRelFileP [PPosix.relfile|pages/Landing|]
                            }
                        }
                    , WaspElementRoute $ R.Route
                        { R._urlPath = "/test"
                        , R._targetPage = "TestPage"
                        }
                    , WaspElementPage $ Wasp.Page.Page
                        { Wasp.Page._name = "TestPage"
                        , Wasp.Page._component = Wasp.JsImport.JsImport
                            { Wasp.JsImport._defaultImport = Just "Test"
                            , Wasp.JsImport._namedImports = []
                            , Wasp.JsImport._from = SP.fromPathRelFileP [PPosix.relfile|pages/Test|]
                            }
                        }
                    , WaspElementEntityPSL $ Wasp.EntityPSL.EntityPSL
                        { Wasp.EntityPSL._name = "Task"
                        , Wasp.EntityPSL._pslModelSchema = "\
                                \id          Int     @id @default(autoincrement())\n\
                            \    description String\n\
                            \    isDone      Boolean @default(false)"
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
                                , EF._fieldLabel = Just Nothing
                                , EF._fieldPlaceholder = Just "What will you do?"
                                }
                            , EF.Field
                                { EF._fieldName = "isDone"
                                , EF._fieldShow = Just False
                                , EF._fieldDefaultValue = Just $ EF.DefaultValueBool False
                                , EF._fieldLabel = Nothing
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
                        , EL._mutexFilters =
                            [ EL.Filter
                                { EL._filterName = "completed"
                                , EL._filterPredicate = Wasp.JsCode.JsCode "task => task.isDone"
                                }
                            , EL.Filter
                                { EL._filterName = "active"
                                , EL._filterPredicate = Wasp.JsCode.JsCode "task => !task.isDone"
                                }
                            ]
                        }
                    , WaspElementQuery $  Wasp.Query.Query
                        { Wasp.Query._name = "myQuery"
                        , Wasp.Query._jsFunction = Wasp.JsImport.JsImport
                            { Wasp.JsImport._defaultImport = Nothing
                            , Wasp.JsImport._namedImports = [ "myJsQuery" ]
                            , Wasp.JsImport._from = SP.fromPathRelFileP [PPosix.relfile|some/path|]
                            }
                        }
                     , WaspElementNpmDependencies $  Wasp.NpmDependencies.NpmDependencies
                        { Wasp.NpmDependencies._dependencies =
                          [ ND.NpmDependency
                            { ND._name = "lodash"
                            , ND._version = "^4.17.15"
                            }
                          ]
                        }
                    ]
                    `setJsImports` [ JsImport (Just "something") [] (SP.fromPathRelFileP [PPosix.relfile|some/file|]) ]
                )
