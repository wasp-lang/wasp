{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Psl.Generator.SchemaTest where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.Type as Psl.Type
import qualified Wasp.Psl.Ast.View as Psl.View
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Psl.Generator.Schema (generateSchema)
import qualified Wasp.Psl.Parser.Schema as Psl.Parser.Schema

prop_generatePslSchema :: Property
prop_generatePslSchema = mapSize (const 100) $ \schemaElementAst ->
  within 1000000 $ do
    let schemaAst = Psl.Schema.Schema [schemaElementAst]
    parse (generateSchema schemaAst) `shouldBe` Right schemaAst
  where
    parse = Megaparsec.parse Psl.Parser.Schema.schema ""

instance Arbitrary Psl.Schema.Block where
  arbitrary =
    oneof
      [ Psl.Schema.ModelBlock <$> arbitrary,
        Psl.Schema.ViewBlock <$> arbitrary,
        Psl.Schema.TypeBlock <$> arbitrary,
        Psl.Schema.EnumBlock <$> arbitrary,
        Psl.Schema.ConfigBlock <$> arbitrary
      ]

instance Arbitrary Psl.Schema.Schema where
  arbitrary = Psl.Schema.Schema <$> scale (const 5) arbitrary

instance Arbitrary Psl.Model.Model where
  arbitrary = Psl.Model.Model <$> arbitraryIdentifier <*> arbitrary

instance Arbitrary Psl.View.View where
  arbitrary = Psl.View.View <$> arbitraryIdentifier <*> arbitrary

instance Arbitrary Psl.Type.Type where
  arbitrary = Psl.Type.Type <$> arbitraryIdentifier <*> arbitrary

instance Arbitrary Psl.Model.Body where
  arbitrary = do
    fieldElement <- Psl.Model.ElementField <$> arbitrary
    elementsBefore <- scale (const 5) arbitrary
    elementsAfter <- scale (const 5) arbitrary

    elementsWithCtx <- mapM withArbitraryCtx (elementsBefore ++ [fieldElement] ++ elementsAfter)
    return $ Psl.Model.Body elementsWithCtx

instance Arbitrary Psl.Model.Element where
  arbitrary =
    oneof
      [ Psl.Model.ElementField <$> arbitrary,
        Psl.Model.ElementBlockAttribute <$> arbitrary
      ]

instance Arbitrary Psl.Model.Field where
  arbitrary = do
    name <- arbitraryIdentifier
    fieldType <- arbitrary
    modifiers <- oneof [(: []) <$> arbitrary, return []]
    attrs <- scale (const 5) arbitrary
    return $
      Psl.Model.Field
        { Psl.Model._name = name,
          Psl.Model._type = fieldType,
          Psl.Model._typeModifiers = modifiers,
          Psl.Model._attrs = attrs
        }

instance Arbitrary Psl.Model.FieldType where
  arbitrary =
    oneof
      [ return Psl.Model.String,
        return Psl.Model.Boolean,
        return Psl.Model.Int,
        return Psl.Model.BigInt,
        return Psl.Model.Float,
        return Psl.Model.Decimal,
        return Psl.Model.DateTime,
        return Psl.Model.Json,
        return Psl.Model.Bytes,
        Psl.Model.Unsupported . show <$> arbitraryIdentifier,
        Psl.Model.UserType <$> arbitraryIdentifier
      ]

instance Arbitrary Psl.Model.FieldTypeModifier where
  arbitrary = oneof [return Psl.Model.List, return Psl.Model.Optional]

instance Arbitrary Psl.Attribute.Attribute where
  arbitrary = do
    name <-
      frequency
        [ (2, arbitraryIdentifier),
          (1, ("db." ++) <$> arbitraryIdentifier)
        ]
    args <- scale (const 5) arbitrary
    return $ Psl.Attribute.Attribute {Psl.Attribute._attrName = name, Psl.Attribute._attrArgs = args}

instance Arbitrary Psl.Argument.Argument where
  arbitrary =
    oneof
      [ Psl.Argument.ArgNamed <$> arbitraryIdentifier <*> arbitrary,
        Psl.Argument.ArgUnnamed <$> arbitrary
      ]

instance Arbitrary Psl.Argument.Expression where
  arbitrary =
    frequency
      [ (5, Psl.Argument.StringExpr <$> arbitraryNonEmptyPrintableString),
        (5, Psl.Argument.IdentifierExpr <$> arbitraryIdentifier),
        (5, Psl.Argument.FuncExpr <$> arbitraryIdentifier <*> scale (const 5) arbitrary),
        -- NOTE: For now we are not supporting negative numbers.
        --   I couldn't figure out from Prisma docs if there could be the case
        --   where these numbers could be negative. Probably we should take care of that case.
        ( 5,
          Psl.Argument.NumberExpr
            <$> oneof
              [ show <$> (arbitrary :: Gen Int) `suchThat` (>= 0),
                show <$> (arbitrary :: Gen Float) `suchThat` (>= 0)
              ]
        ),
        -- We want to generate arrays with a smaller frequency, because they can be nested and
        -- tests with nested arrays take too long to run.
        (1, Psl.Argument.ArrayExpr <$> scale (const 5) (listOf1 arbitrary))
      ]

instance Arbitrary Psl.Enum.Enum where
  arbitrary = do
    name <- arbitraryIdentifier
    values <- scale (const 5) (listOf1 $ withArbitraryCtx =<< arbitrary)
    return $ Psl.Enum.Enum name values

instance Arbitrary Psl.Enum.Element where
  arbitrary =
    oneof
      [ Psl.Enum.ElementValue <$> arbitraryIdentifier <*> scale (const 5) arbitrary,
        Psl.Enum.ElementBlockAttribute <$> arbitrary
      ]

instance Arbitrary Psl.ConfigBlock.ConfigBlock where
  arbitrary = do
    name <- arbitraryIdentifier
    config <- scale (const 5) arbitrary
    configBlockType <-
      oneof
        [ return Psl.ConfigBlock.Datasource,
          return Psl.ConfigBlock.Generator
        ]
    return $ Psl.ConfigBlock.ConfigBlock configBlockType name config

instance Arbitrary Psl.ConfigBlock.KeyValuePair where
  arbitrary = Psl.ConfigBlock.KeyValuePair <$> arbitraryIdentifier <*> arbitrary

instance Arbitrary a => Arbitrary (Psl.WithCtx.WithCtx a) where
  arbitrary = arbitrary >>= withArbitraryCtx

withArbitraryCtx :: node -> Gen (Psl.WithCtx.WithCtx node)
withArbitraryCtx node = Psl.WithCtx.WithCtx node <$> arbitrary

instance Arbitrary Psl.WithCtx.NodeContext where
  arbitrary = do
    documentationComments <- scale (const 5) $ listOf arbitraryPrintableSingleLine

    return $
      Psl.WithCtx.NodeContext
        { Psl.WithCtx.documentationComments = documentationComments
        }

arbitraryNonEmptyPrintableString :: Gen String
arbitraryNonEmptyPrintableString = listOf1 arbitraryPrintableChar

arbitraryPrintableSingleLine :: Gen String
arbitraryPrintableSingleLine = listOf $ arbitraryAlphaNum `suchThat` (/= '\n')

arbitraryAlpha :: Gen Char
arbitraryAlpha = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

arbitraryAlphaNum :: Gen Char
arbitraryAlphaNum = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

arbitraryIdentifier :: Gen String
arbitraryIdentifier = (:) <$> arbitraryAlpha <*> listOf arbitraryAlphaNum
