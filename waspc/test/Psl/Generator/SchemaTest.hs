{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Psl.Generator.SchemaTest where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Schema as AST
import Wasp.Psl.Generator.Schema (generateSchemaElement)
import qualified Wasp.Psl.Parser.Schema as Psl.Parser.Schema

prop_generatePslSchema :: Property
prop_generatePslSchema = mapSize (const 100) $ \schemaElementAst ->
  within 1000000 $
    Parsec.parse Psl.Parser.Schema.schema "" (generateSchemaElement schemaElementAst) `shouldBe` Right (AST.Schema [schemaElementAst])

instance Arbitrary AST.SchemaElement where
  arbitrary =
    oneof
      [ AST.SchemaModel <$> arbitrary,
        AST.SchemaEnum <$> arbitrary,
        AST.SchemaDatasource <$> arbitrary,
        AST.SchemaGenerator <$> arbitrary
      ]

instance Arbitrary AST.Schema where
  arbitrary = AST.Schema <$> scale (const 5) arbitrary

instance Arbitrary AST.Model where
  arbitrary = AST.Model <$> arbitraryIdentifier <*> arbitrary

instance Arbitrary AST.Body where
  arbitrary = do
    fieldElement <- AST.ElementField <$> arbitrary
    elementsBefore <- scale (const 5) arbitrary
    elementsAfter <- scale (const 5) arbitrary
    return $ AST.Body $ elementsBefore ++ [fieldElement] ++ elementsAfter

instance Arbitrary AST.Element where
  arbitrary =
    oneof
      [ AST.ElementField <$> arbitrary,
        AST.ElementBlockAttribute <$> arbitrary
      ]

instance Arbitrary AST.Field where
  arbitrary = do
    name <- arbitraryIdentifier
    fieldType <- arbitrary
    modifiers <- oneof [(: []) <$> arbitrary, return []]
    attrs <- scale (const 5) arbitrary
    return $
      AST.Field
        { AST._name = name,
          AST._type = fieldType,
          AST._typeModifiers = modifiers,
          AST._attrs = attrs
        }

instance Arbitrary AST.FieldType where
  arbitrary =
    oneof
      [ return AST.String,
        return AST.Boolean,
        return AST.Int,
        return AST.BigInt,
        return AST.Float,
        return AST.Decimal,
        return AST.DateTime,
        return AST.Json,
        return AST.Bytes,
        AST.Unsupported . show <$> arbitraryIdentifier,
        AST.UserType <$> arbitraryIdentifier
      ]

instance Arbitrary AST.FieldTypeModifier where
  arbitrary = oneof [return AST.List, return AST.Optional]

instance Arbitrary AST.Attribute where
  arbitrary = do
    name <-
      frequency
        [ (2, arbitraryIdentifier),
          (1, ("db." ++) <$> arbitraryIdentifier)
        ]
    args <- scale (const 5) arbitrary
    return $ AST.Attribute {AST._attrName = name, AST._attrArgs = args}

instance Arbitrary AST.AttributeArg where
  arbitrary =
    oneof
      [ AST.AttrArgNamed <$> arbitraryIdentifier <*> arbitrary,
        AST.AttrArgUnnamed <$> arbitrary
      ]

instance Arbitrary AST.AttrArgValue where
  arbitrary =
    oneof
      [ AST.AttrArgString <$> arbitraryNonEmptyPrintableString,
        AST.AttrArgIdentifier <$> arbitraryIdentifier,
        AST.AttrArgFunc <$> arbitraryIdentifier,
        AST.AttrArgFieldRefList <$> scale (const 5) (listOf1 arbitraryIdentifier),
        -- NOTE: For now we are not supporting negative numbers.
        --   I couldn't figure out from Prisma docs if there could be the case
        --   where these numbers could be negative. Probably we should take care of that case.
        AST.AttrArgNumber
          <$> oneof
            [ show <$> (arbitrary :: Gen Int) `suchThat` (>= 0),
              show <$> (arbitrary :: Gen Float) `suchThat` (>= 0)
            ]
            -- NOTE: Unknown is commented out because unknown should contain only values
            --   that are not recognized as any other type of attribute argument,
            --   and defining how those are generated is not so simple, so I skipped it for now.
            -- , AST.AttrArgUnknown <$> arbitraryNonEmptyPrintableString
      ]

instance Arbitrary AST.PrismaEnum where
  arbitrary = do
    name <- arbitraryIdentifier
    values <- scale (const 5) (listOf1 arbitraryIdentifier)
    return $ AST.PrismaEnum name values

instance Arbitrary AST.Datasource where
  arbitrary = do
    name <- arbitraryIdentifier
    config <- scale (const 5) arbitrary
    return $ AST.Datasource name config

instance Arbitrary AST.Generator where
  arbitrary = do
    name <- arbitraryIdentifier
    config <- scale (const 5) arbitrary
    return $ AST.Generator name config

instance Arbitrary AST.ConfigBlockKeyValue where
  arbitrary = AST.ConfigBlockKeyValue <$> arbitraryIdentifier <*> arbitraryIdentifier

arbitraryNonEmptyPrintableString :: Gen String
arbitraryNonEmptyPrintableString = listOf1 arbitraryPrintableChar

arbitraryAlpha :: Gen Char
arbitraryAlpha = elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

arbitraryAlphaNum :: Gen Char
arbitraryAlphaNum = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

arbitraryIdentifier :: Gen String
arbitraryIdentifier = (:) <$> arbitraryAlpha <*> listOf arbitraryAlphaNum
