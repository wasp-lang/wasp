{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Psl.Generator.ModelTest where

import Psl.Common.ModelTest (sampleBodyAst)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Generator.Schema (generateSchemaElement)
import qualified Wasp.Psl.Parser.Model as Psl.Parser.Model

spec_generatePslModel :: Spec
spec_generatePslModel = do
  describe "Complex example" $ do
    let pslModelAst = Psl.Schema.SchemaModel $ Psl.Model.Model "User" sampleBodyAst

    it "parse(generate(sampleBodyAst)) == sampleBodyAst" $ do
      Parsec.parse Psl.Parser.Model.model "" (generateSchemaElement pslModelAst) `shouldBe` Right pslModelAst
