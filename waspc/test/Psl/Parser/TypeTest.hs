module Psl.Parser.TypeTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Type as Psl.Type
import qualified Wasp.Psl.Parser.Type as Psl.Parser

spec_parsePslType :: Spec
spec_parsePslType = do
  describe "Type parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                type Photo {
                  height Int    @default(200)
                  width  Int    @default(100)
                  url    String
                } 
              |]
          expectedAst =
            Psl.Type.Type
              "Photo"
              ( Psl.Model.Body
                  [ pure $
                      Psl.Model.ElementField
                        ( Psl.Model.Field
                            "height"
                            Psl.Model.Int
                            []
                            [ Psl.Attribute.Attribute
                                "default"
                                [ Psl.Argument.ArgUnnamed (Psl.Argument.NumberExpr "200")
                                ]
                            ]
                        ),
                    pure $
                      Psl.Model.ElementField
                        ( Psl.Model.Field
                            "width"
                            Psl.Model.Int
                            []
                            [ Psl.Attribute.Attribute
                                "default"
                                [ Psl.Argument.ArgUnnamed (Psl.Argument.NumberExpr "100")
                                ]
                            ]
                        ),
                    pure $
                      Psl.Model.ElementField
                        ( Psl.Model.Field "url" Psl.Model.String [] []
                        )
                  ]
              )

      Parsec.parse Psl.Parser.typeBlock "" source `shouldBe` Right expectedAst

    it "Commented out fields" $ do
      let source =
            T.unpack
              [trimming|
                type Photo {
                  height Int    @default(200)
                  // width  Int    @default(100)
                  url    String
                } 
              |]
          expectedAst =
            Psl.Type.Type
              "Photo"
              ( Psl.Model.Body
                  [ pure $
                      Psl.Model.ElementField
                        ( Psl.Model.Field
                            "height"
                            Psl.Model.Int
                            []
                            [ Psl.Attribute.Attribute
                                "default"
                                [ Psl.Argument.ArgUnnamed (Psl.Argument.NumberExpr "200")
                                ]
                            ]
                        ),
                    pure $
                      Psl.Model.ElementField
                        ( Psl.Model.Field "url" Psl.Model.String [] []
                        )
                  ]
              )

      Parsec.parse Psl.Parser.typeBlock "" source `shouldBe` Right expectedAst
