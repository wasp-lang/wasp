module AppSpec.EntityTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.Entity (getPrimaryKeyField)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Ast.Model as PslModel

spec_AppSpecEntityTest :: Spec
spec_AppSpecEntityTest = do
  describe "getPrimaryField" $ do
    it "gets primary field from entity when it exists" $ do
      getPrimaryKeyField entityWithPrimaryField `shouldBe` Just primaryField
    it "returns Nothing if primary field doesn't exist" $ do
      getPrimaryKeyField entityWithoutPrimaryField `shouldBe` Nothing
  where
    entityWithPrimaryField =
      Entity.makeEntity $
        PslModel.Body
          [ PslModel.ElementField primaryField,
            PslModel.ElementField someOtherField
          ]
    entityWithoutPrimaryField =
      Entity.makeEntity $
        PslModel.Body
          [ PslModel.ElementField someOtherField
          ]

    primaryField =
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
