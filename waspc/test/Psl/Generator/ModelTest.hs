{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Psl.Generator.ModelTest where

import Psl.Common.ModelTest (sampleBodyAst)
import Test.Tasty.Hspec
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Generator.Schema (generateSchemaBlock)
import qualified Wasp.Psl.Parser.Model as Psl.Parser.Model

spec_generatePslModel :: Spec
spec_generatePslModel = do
  describe "Complex example" $ do
    let pslModelAst = Psl.Model.Model "User" sampleBodyAst

    it "parse(generate(sampleBodyAst)) == sampleBodyAst" $ do
      Megaparsec.parse Psl.Parser.Model.model "" (generateSchemaBlock $ Psl.Schema.ModelBlock pslModelAst) `shouldBe` Right pslModelAst
