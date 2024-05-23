module AppSpec.EntityTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.Entity (getIdField)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

spec_AppSpecEntityTest :: Spec
spec_AppSpecEntityTest = do
  describe "getIdField" $ do
    it "Gets primary field from entity when it exists" $ do
      getIdField entityWithIdField `shouldBe` Just idField
    it "Returns Nothing if primary field doesn't exist" $ do
      getIdField entityWithoutIdField `shouldBe` Nothing

  describe "isFieldUnique" $ do
    it "Returns Nothing if the field doesn't exist on the entity" $ do
      Entity.isFieldUnique "nonExistingField" entityWithoutIdField `shouldBe` Nothing
    it "Returns Just False if the field exists on the entity but isn't unique" $ do
      Entity.isFieldUnique "description" entityWithIdField `shouldBe` Just False
    it "Returns Just True if the field exists and is unique" $ do
      Entity.isFieldUnique "id" entityWithIdField `shouldBe` Just True

  describe "doesFieldHaveAttribute" $ do
    it "Returns Nothing if the field doesn't exist on the entity" $ do
      Entity.doesFieldHaveAttribute "nonExistingField" "unique" entityWithoutIdField `shouldBe` Nothing
    it "Returns Just False if the field exists on the entity but doesn't have the required attribute" $ do
      Entity.doesFieldHaveAttribute "description" "id" entityWithIdField `shouldBe` Just False
    it "Returns Just True if the field exists on the entity and has the required attribute" $ do
      Entity.doesFieldHaveAttribute "id" "id" entityWithIdField `shouldBe` Just True
  where
    entityWithIdField =
      Entity.makeEntity $
        Psl.Ast.Body
          [ Psl.Ast.ElementField idField,
            Psl.Ast.ElementField someOtherField
          ]
    entityWithoutIdField =
      Entity.makeEntity $
        Psl.Ast.Body
          [ Psl.Ast.ElementField someOtherField
          ]

    idField =
      Psl.Ast.Field
        { Psl.Ast._name = "id",
          Psl.Ast._type = Psl.Ast.Int,
          Psl.Ast._attrs =
            [ Psl.Ast.Attribute
                { Psl.Ast._attrName = "id",
                  Psl.Ast._attrArgs = []
                },
              Psl.Ast.Attribute
                { Psl.Ast._attrName = "unique",
                  Psl.Ast._attrArgs = []
                }
            ],
          Psl.Ast._typeModifiers = []
        }

    someOtherField =
      Psl.Ast.Field
        { Psl.Ast._name = "description",
          Psl.Ast._type = Psl.Ast.String,
          Psl.Ast._attrs = [],
          Psl.Ast._typeModifiers = []
        }
