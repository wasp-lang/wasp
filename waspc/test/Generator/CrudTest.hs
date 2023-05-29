module Generator.CrudTest where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson
import StrongPath (relfileP)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Psl.Ast.Model (Field (_typeModifiers))
import qualified Wasp.Psl.Ast.Model as PslModel

spec_GeneratorCrudTest :: Spec
spec_GeneratorCrudTest = do
  describe "getCrudOperationJson" $ do
    it "it makes all operations disabled by default" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            operations =
              AS.Crud.CrudOperations
                { get = Nothing,
                  getAll = Nothing,
                  create = Nothing,
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson (object [])

    it "it adds JSON for enabled operations" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            operations =
              AS.Crud.CrudOperations
                { get =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Nothing,
                            overrideFn = Nothing
                          }
                      ),
                  getAll =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Nothing,
                            overrideFn = Nothing
                          }
                      ),
                  create =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Nothing,
                            overrideFn = Nothing
                          }
                      ),
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "route" .= ("get" :: String),
                      "fullPath" .= ("tasks/get" :: String),
                      "isPublic" .= False
                    ],
                "GetAll"
                  .= object
                    [ "route" .= ("get-all" :: String),
                      "fullPath" .= ("tasks/get-all" :: String),
                      "isPublic" .= False
                    ],
                "Create"
                  .= object
                    [ "route" .= ("create" :: String),
                      "fullPath" .= ("tasks/create" :: String),
                      "isPublic" .= False
                    ]
              ]
          )

    it "it makes some operations public" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            operations =
              AS.Crud.CrudOperations
                { get =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Just True,
                            overrideFn = Nothing
                          }
                      ),
                  getAll =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Just False,
                            overrideFn = Nothing
                          }
                      ),
                  create =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Just True,
                            overrideFn = Nothing
                          }
                      ),
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "route" .= ("get" :: String),
                      "fullPath" .= ("tasks/get" :: String),
                      "isPublic" .= True
                    ],
                "GetAll"
                  .= object
                    [ "route" .= ("get-all" :: String),
                      "fullPath" .= ("tasks/get-all" :: String),
                      "isPublic" .= False
                    ],
                "Create"
                  .= object
                    [ "route" .= ("create" :: String),
                      "fullPath" .= ("tasks/create" :: String),
                      "isPublic" .= True
                    ]
              ]
          )

    it "the override import can defined" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntitName,
            operations =
              AS.Crud.CrudOperations
                { get =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Just True,
                            overrideFn =
                              Just $
                                AS.ExtImport.ExtImport
                                  { AS.ExtImport.name = AS.ExtImport.ExtImportField "getTask",
                                    AS.ExtImport.path = SP.castRel [relfileP|bla/tasks.js|]
                                  }
                          }
                      ),
                  getAll =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Just False,
                            overrideFn = Nothing
                          }
                      ),
                  create =
                    Just
                      ( AS.Crud.CrudOperationOptions
                          { isPublic = Just True,
                            overrideFn = Nothing
                          }
                      ),
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          ( object
              [ "Get"
                  .= object
                    [ "route" .= ("get" :: String),
                      "fullPath" .= ("tasks/get" :: String),
                      "isPublic" .= True
                    ],
                "GetAll"
                  .= object
                    [ "route" .= ("get-all" :: String),
                      "fullPath" .= ("tasks/get-all" :: String),
                      "isPublic" .= False
                    ],
                "Create"
                  .= object
                    [ "route" .= ("create" :: String),
                      "fullPath" .= ("tasks/create" :: String),
                      "isPublic" .= True
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
