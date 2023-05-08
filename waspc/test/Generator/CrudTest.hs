module Generator.CrudTest where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson
import Test.Tasty.Hspec
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.Generator.Crud (getCrudOperationJson)

spec_GeneratorCrudTest :: Spec
spec_GeneratorCrudTest = do
  describe "getCrudOperationJson" $ do
    it "it makes all operations enabled by default" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Nothing,
            except = Nothing
          }
        `shouldBe` mkOperationsJson
          ( object
              [ "Get" .= True,
                "GetAll" .= True,
                "Create" .= True,
                "Update" .= True,
                "Delete" .= True
              ]
          )

    it "it only enables operations defined in the only property" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Just [AS.Crud.Get, AS.Crud.GetAll],
            except = Nothing
          }
        `shouldBe` mkOperationsJson
          ( object
              [ "Get" .= True,
                "GetAll" .= True,
                "Create" .= False,
                "Update" .= False,
                "Delete" .= False
              ]
          )

    it "it only enables all operations except those defined in the except property" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Nothing,
            except = Just [AS.Crud.Delete]
          }
        `shouldBe` mkOperationsJson
          ( object
              [ "Get" .= True,
                "GetAll" .= True,
                "Create" .= True,
                "Update" .= True,
                "Delete" .= False
              ]
          )
  where
    crudOperationsName :: String
    crudOperationsName = "tasksCrud"

    crudOperationEntitName = "Task"

    mkOperationsJson :: Data.Aeson.Value -> Data.Aeson.Value
    mkOperationsJson enabledOperations =
      object
        [ "name" .= crudOperationsName,
          "enabledOperations" .= enabledOperations,
          "entityLower" .= ("task" :: String),
          "entityUpper" .= ("Task" :: String)
        ]
