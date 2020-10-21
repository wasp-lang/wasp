module Parser.ParserTest where

import           Data.Either
import qualified Path.Posix           as PPosix
import           Test.Tasty.Hspec

import           NpmDependency        as ND
import           Parser
import qualified StrongPath           as SP
import           Wasp
import qualified Wasp.Entity
import qualified Wasp.Auth
import qualified Wasp.JsImport
import qualified Wasp.NpmDependencies
import qualified Wasp.Page
import qualified Wasp.Query
import qualified Wasp.Route           as R

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
                    , WaspElementAuth $ Wasp.Auth.Auth
                        { Wasp.Auth._userEntity = "User"
                        , Wasp.Auth._methods = [Wasp.Auth.EmailAndPassword]
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
                    , WaspElementEntity $ Wasp.Entity.Entity
                        { Wasp.Entity._name = "Task"
                        , Wasp.Entity._pslModelSchema = "\
                                \id          Int     @id @default(autoincrement())\n\
                            \    description String\n\
                            \    isDone      Boolean @default(false)"
                        }
                    , WaspElementQuery $  Wasp.Query.Query
                        { Wasp.Query._name = "myQuery"
                        , Wasp.Query._jsFunction = Wasp.JsImport.JsImport
                            { Wasp.JsImport._defaultImport = Nothing
                            , Wasp.JsImport._namedImports = [ "myJsQuery" ]
                            , Wasp.JsImport._from = SP.fromPathRelFileP [PPosix.relfile|some/path|]
                            }
                        , Wasp.Query._entities = Nothing
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
