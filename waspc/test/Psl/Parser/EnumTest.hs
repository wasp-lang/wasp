module Psl.Parser.EnumTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Schema as Psl.Ast
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
            Psl.Ast.SchemaEnum $
              Psl.Ast.PrismaEnum
                "Role"
                [ Psl.Ast.EnumValue "USER" [],
                  Psl.Ast.EnumValue
                    "ADMIN"
                    [ Psl.Ast.Attribute
                        "map"
                        [Psl.Ast.AttrArgUnnamed $ Psl.Ast.AttrArgString "ADMIN_MAPPING"]
                    ],
                  Psl.Ast.EnumBlockAttribute $
                    Psl.Ast.Attribute
                      "map"
                      [Psl.Ast.AttrArgUnnamed $ Psl.Ast.AttrArgString "enum_name"]
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
            Psl.Ast.SchemaEnum $
              Psl.Ast.PrismaEnum
                "Role"
                [ Psl.Ast.EnumValue "USER" [],
                  Psl.Ast.EnumBlockAttribute $
                    Psl.Ast.Attribute
                      "map"
                      [Psl.Ast.AttrArgUnnamed $ Psl.Ast.AttrArgString "enum_name"]
                ]
      Parsec.parse Psl.Parser.enum "" source `shouldBe` Right expectedAst
