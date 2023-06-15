module AppSpec.EntityTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.Entity (getIdField)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Ast.Model as PslModel

spec_AppSpecEntityTest :: Spec
spec_AppSpecEntityTest = do
  describe "getIdField" $ do
    it "gets primary field from entity when it exists" $ do
      getIdField entityWithIdField `shouldBe` Just idField
    it "returns Nothing if primary field doesn't exist" $ do
      getIdField entityWithoutIdField `shouldBe` Nothing
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
