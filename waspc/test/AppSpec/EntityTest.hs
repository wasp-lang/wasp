module AppSpec.EntityTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.Entity (getIdField)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model

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
        Psl.Model.Body
          [ Psl.Model.ElementField idField,
            Psl.Model.ElementField someOtherField
          ]
    entityWithoutIdField =
      Entity.makeEntity $
        Psl.Model.Body
          [ Psl.Model.ElementField someOtherField
          ]

    idField =
      Psl.Model.Field
        { Psl.Model._name = "id",
          Psl.Model._type = Psl.Model.Int,
          Psl.Model._attrs =
            [ Psl.Attribute.Attribute
                { Psl.Attribute._attrName = "id",
                  Psl.Attribute._attrArgs = []
                },
              Psl.Attribute.Attribute
                { Psl.Attribute._attrName = "unique",
                  Psl.Attribute._attrArgs = []
                }
            ],
          Psl.Model._typeModifiers = []
        }

    someOtherField =
      Psl.Model.Field
        { Psl.Model._name = "description",
          Psl.Model._type = Psl.Model.String,
          Psl.Model._attrs = [],
          Psl.Model._typeModifiers = []
        }
