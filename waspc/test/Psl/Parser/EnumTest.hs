module Psl.Parser.EnumTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.Enum as Psl.Parser

spec_parsePslEnum :: Spec
spec_parsePslEnum = do
  describe "Enum parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                enum Role {
                  USER
                  ADMIN @map("ADMIN_MAPPING")

                  @@map("enum_name")
                }
              |]
          expectedAst =
            Psl.Schema.SchemaEnum $
              Psl.Enum.Enum
                "Role"
                [ Psl.Enum.EnumElementValue "USER" [],
                  Psl.Enum.EnumElementValue
                    "ADMIN"
                    [ Psl.Attribute.Attribute
                        "map"
                        [Psl.Attribute.AttrArgUnnamed $ Psl.Attribute.AttrArgString "ADMIN_MAPPING"]
                    ],
                  Psl.Enum.EnumElementBlockAttribute $
                    Psl.Attribute.Attribute
                      "map"
                      [Psl.Attribute.AttrArgUnnamed $ Psl.Attribute.AttrArgString "enum_name"]
                ]
      Parsec.parse Psl.Parser.enum "" source `shouldBe` Right expectedAst

    it "Commented out fields" $ do
      let source =
            T.unpack
              [trimming|
                enum Role {
                  USER
                  // ADMIN @map("ADMIN_MAPPING")

                  @@map("enum_name")
                }
              |]
          expectedAst =
            Psl.Schema.SchemaEnum $
              Psl.Enum.Enum
                "Role"
                [ Psl.Enum.EnumElementValue "USER" [],
                  Psl.Enum.EnumElementBlockAttribute $
                    Psl.Attribute.Attribute
                      "map"
                      [Psl.Attribute.AttrArgUnnamed $ Psl.Attribute.AttrArgString "enum_name"]
                ]
      Parsec.parse Psl.Parser.enum "" source `shouldBe` Right expectedAst
