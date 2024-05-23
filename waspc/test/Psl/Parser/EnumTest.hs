module Psl.Parser.EnumTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Schema as AST
import qualified Wasp.Psl.Parser.Enum as Psl.Parser

spec_parsePslEnum :: Spec
spec_parsePslEnum = do
  describe "Enum parsing" $ do
    let source =
          T.unpack
            [trimming|
              enum Role {
                USER
                ADMIN
              }
            |]
        expectedAst = AST.SchemaEnum $ AST.PrismaEnum "Role" ["USER", "ADMIN"]

    it "Enum is correctly parsed" $ do
      Parsec.parse Psl.Parser.enum "" source `shouldBe` Right expectedAst
