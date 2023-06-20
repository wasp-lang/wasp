module AppSpec.EntityTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.Entity (getIdField)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Ast.Model as PslModel

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
        PslModel.Body
          [ PslModel.ElementField idField,
            PslModel.ElementField someOtherField
          ]
    entityWithoutIdField =
      Entity.makeEntity $
        PslModel.Body
          [ PslModel.ElementField someOtherField
          ]

    idField =
      PslModel.Field
        { PslModel._name = "id",
          PslModel._type = PslModel.Int,
          PslModel._attrs =
            [ PslModel.Attribute
                { PslModel._attrName = "id",
                  PslModel._attrArgs = []
                },
              PslModel.Attribute
                { PslModel._attrName = "unique",
                  PslModel._attrArgs = []
                }
            ],
          PslModel._typeModifiers = []
        }

    someOtherField =
      PslModel.Field
        { PslModel._name = "description",
          PslModel._type = PslModel.String,
          PslModel._attrs = [],
          PslModel._typeModifiers = []
        }
