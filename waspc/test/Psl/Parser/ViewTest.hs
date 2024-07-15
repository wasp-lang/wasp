module Psl.Parser.ViewTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.View as Psl.View
import qualified Wasp.Psl.Parser.View as Psl.Parser

spec_parsePslView :: Spec
spec_parsePslView = do
  describe "View parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                view UserInfo {
                  id    Int?
                  email String?
                  name  String?
                  bio   String?

                  @@ignore
                }
              |]
          expectedAst =
            Psl.View.View
              "UserInfo"
              ( Psl.Model.Body
                  [ Psl.Model.ElementField
                      ( Psl.Model.Field "id" Psl.Model.Int [Psl.Model.Optional] []
                      ),
                    Psl.Model.ElementField
                      ( Psl.Model.Field "email" Psl.Model.String [Psl.Model.Optional] []
                      ),
                    Psl.Model.ElementField
                      ( Psl.Model.Field "name" Psl.Model.String [Psl.Model.Optional] []
                      ),
                    Psl.Model.ElementField
                      ( Psl.Model.Field "bio" Psl.Model.String [Psl.Model.Optional] []
                      ),
                    Psl.Model.ElementBlockAttribute
                      ( Psl.Attribute.Attribute "ignore" []
                      )
                  ]
              )

      Parsec.parse Psl.Parser.view "" source `shouldBe` Right expectedAst

    it "Commented out fields" $ do
      let source =
            T.unpack
              [trimming|
                view UserInfo {
                  id    Int?
                  email String?
                  // name  String?
                  bio   String?

                  @@ignore
                } 
              |]
          expectedAst =
            Psl.View.View
              "UserInfo"
              ( Psl.Model.Body
                  [ Psl.Model.ElementField
                      ( Psl.Model.Field
                          "id"
                          Psl.Model.Int
                          [ Psl.Model.Optional
                          ]
                          []
                      ),
                    Psl.Model.ElementField
                      ( Psl.Model.Field
                          "email"
                          Psl.Model.String
                          [ Psl.Model.Optional
                          ]
                          []
                      ),
                    Psl.Model.ElementField
                      ( Psl.Model.Field
                          "bio"
                          Psl.Model.String
                          [ Psl.Model.Optional
                          ]
                          []
                      ),
                    Psl.Model.ElementBlockAttribute
                      ( Psl.Attribute.Attribute "ignore" []
                      )
                  ]
              )

      Parsec.parse Psl.Parser.view "" source `shouldBe` Right expectedAst
