module Psl.Parser.EnumTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import qualified Wasp.Psl.Parser.Enum as Psl.Parser

spec_parsePslEnum :: Spec
spec_parsePslEnum = do
  describe "Enum parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                enum Role {
                  USER // inline comments
                  ADMIN @map("ADMIN_MAPPING")

                  @@map("enum_name")
                }
              |]
          expectedAst =
            Psl.Enum.Enum
              "Role"
              $ Psl.WithCtx.empty
                <$> [ Psl.Enum.ElementValue "USER" [],
                      Psl.Enum.ElementValue
                        "ADMIN"
                        [ Psl.Attribute.Attribute
                            "map"
                            [Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "ADMIN_MAPPING"]
                        ],
                      Psl.Enum.ElementBlockAttribute $
                        Psl.Attribute.Attribute
                          "map"
                          [Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "enum_name"]
                    ]
      Megaparsec.parse Psl.Parser.enum "" source `shouldBe` Right expectedAst

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
            Psl.Enum.Enum
              "Role"
              $ Psl.WithCtx.empty
                <$> [ Psl.Enum.ElementValue "USER" [],
                      Psl.Enum.ElementBlockAttribute $
                        Psl.Attribute.Attribute
                          "map"
                          [Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "enum_name"]
                    ]
      Megaparsec.parse Psl.Parser.enum "" source `shouldBe` Right expectedAst
