module AppSpec.EntityTest where

import Test.Tasty.Hspec
import Wasp.AppSpec.Entity (getIdField)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx

spec_AppSpecEntityTest :: Spec
spec_AppSpecEntityTest = do
  describe "getIdField" $ do
    it "Gets primary field from entity when it exists" $ do
      getIdField entityWithIdField `shouldBe` Just idField
    it "Returns Nothing if primary field doesn't exist" $ do
      getIdField entityWithoutIdField `shouldBe` Nothing
  where
    entityWithIdField =
      Entity.makeEntity $
        Psl.Model.Body $
          Psl.WithCtx.empty
            <$> [ Psl.Model.ElementField idField,
                  Psl.Model.ElementField someOtherField
                ]
    entityWithoutIdField =
      Entity.makeEntity $
        Psl.Model.Body $
          Psl.WithCtx.empty
            <$> [ Psl.Model.ElementField someOtherField
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
