module Parser.ServerTest where

import Data.Either (isLeft)
import Parser.Common (runWaspParser)
import Parser.Server (server)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Wasp.JsImport
import qualified Wasp.Server

spec_parseServer :: Spec
spec_parseServer =
  describe "Parsing server declaration" $ do
    let parseServer = runWaspParser server

    it "When given a valid server declaration, returns correct AST" $ do
      let setupFnName = "myServerSetup"
          setupFnFrom = [SP.relfileP|some/path|]
      let ast =
            Wasp.Server.Server
              { Wasp.Server._setupJsFunction =
                  Wasp.JsImport.JsImport
                    { Wasp.JsImport._defaultImport = Nothing,
                      Wasp.JsImport._namedImports = [setupFnName],
                      Wasp.JsImport._from = setupFnFrom
                    }
              }
      parseServer
        ( "server {\n"
            ++ "  setupFn: import { "
            ++ setupFnName
            ++ " } from \"@ext/"
            ++ SP.fromRelFileP setupFnFrom
            ++ "\"\n"
            ++ "}"
        )
        `shouldBe` Right ast

    it "When given server wasp declaration without 'serverFn' property, should return Left" $ do
      isLeft (parseServer "server { }") `shouldBe` True
