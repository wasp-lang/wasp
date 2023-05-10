module Generator.CrudTest where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson
import Test.Tasty.Hspec
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Psl.Ast.Model (Field (_typeModifiers))
import qualified Wasp.Psl.Ast.Model as PslModel

spec_GeneratorCrudTest :: Spec
spec_GeneratorCrudTest = do
  describe "getCrudOperationJson" $ do
    it "it makes all operations enabled and private by default" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Nothing,
            except = Nothing,
            public = Nothing
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("get" :: String),
                      "fullPath" .= ("crud/tasks/get" :: String),
                      "isPublic" .= False
                    ],
                "GetAll"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("getAll" :: String),
                      "fullPath" .= ("crud/tasks/getAll" :: String),
                      "isPublic" .= False
                    ],
                "Create"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("create" :: String),
                      "fullPath" .= ("crud/tasks/create" :: String),
                      "isPublic" .= False
                    ],
                "Update"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("update" :: String),
                      "fullPath" .= ("crud/tasks/update" :: String),
                      "isPublic" .= False
                    ],
                "Delete"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("delete" :: String),
                      "fullPath" .= ("crud/tasks/delete" :: String),
                      "isPublic" .= False
                    ]
              ]
          )

    it "it only enables operations defined in the only property" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Just [AS.Crud.Get, AS.Crud.GetAll],
            except = Nothing,
            public = Nothing
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("get" :: String),
                      "fullPath" .= ("crud/tasks/get" :: String),
                      "isPublic" .= False
                    ],
                "GetAll"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("getAll" :: String),
                      "fullPath" .= ("crud/tasks/getAll" :: String),
                      "isPublic" .= False
                    ],
                "Create"
                  .= object
                    [ "isEnabled" .= False,
                      "route" .= ("create" :: String),
                      "fullPath" .= ("crud/tasks/create" :: String),
                      "isPublic" .= False
                    ],
                "Update"
                  .= object
                    [ "isEnabled" .= False,
                      "route" .= ("update" :: String),
                      "fullPath" .= ("crud/tasks/update" :: String),
                      "isPublic" .= False
                    ],
                "Delete"
                  .= object
                    [ "isEnabled" .= False,
                      "route" .= ("delete" :: String),
                      "fullPath" .= ("crud/tasks/delete" :: String),
                      "isPublic" .= False
                    ]
              ]
          )

    it "it only enables all operations except those defined in the except property" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Nothing,
            except = Just [AS.Crud.Delete],
            public = Nothing
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("get" :: String),
                      "fullPath" .= ("crud/tasks/get" :: String),
                      "isPublic" .= False
                    ],
                "GetAll"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("getAll" :: String),
                      "fullPath" .= ("crud/tasks/getAll" :: String),
                      "isPublic" .= False
                    ],
                "Create"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("create" :: String),
                      "fullPath" .= ("crud/tasks/create" :: String),
                      "isPublic" .= False
                    ],
                "Update"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("update" :: String),
                      "fullPath" .= ("crud/tasks/update" :: String),
                      "isPublic" .= False
                    ],
                "Delete"
                  .= object
                    [ "isEnabled" .= False,
                      "route" .= ("delete" :: String),
                      "fullPath" .= ("crud/tasks/delete" :: String),
                      "isPublic" .= False
                    ]
              ]
          )

    it "it makes some operations public" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            only = Nothing,
            except = Nothing,
            public = Just [AS.Crud.Get, AS.Crud.GetAll]
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("get" :: String),
                      "fullPath" .= ("crud/tasks/get" :: String),
                      "isPublic" .= True
                    ],
                "GetAll"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("getAll" :: String),
                      "fullPath" .= ("crud/tasks/getAll" :: String),
                      "isPublic" .= True
                    ],
                "Create"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("create" :: String),
                      "fullPath" .= ("crud/tasks/create" :: String),
                      "isPublic" .= False
                    ],
                "Update"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("update" :: String),
                      "fullPath" .= ("crud/tasks/update" :: String),
                      "isPublic" .= False
                    ],
                "Delete"
                  .= object
                    [ "isEnabled" .= True,
                      "route" .= ("delete" :: String),
                      "fullPath" .= ("crud/tasks/delete" :: String),
                      "isPublic" .= False
                    ]
              ]
          )
  where
    crudOperationsName = "tasks"
    crudOperationEntitName = "Task"
    primaryEntityField =
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

    mkOperationsJson :: Data.Aeson.Value -> Data.Aeson.Value
    mkOperationsJson operations =
      object
        [ "name" .= crudOperationsName,
          "operations" .= operations,
          "entitiesArray" .= ("['Task']" :: String),
          "primaryFieldName" .= ("id" :: String),
          "entityLower" .= ("task" :: String),
          "entityUpper" .= ("Task" :: String)
        ]
