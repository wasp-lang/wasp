{-# LANGUAGE PartialTypeSignatures #-}

module Generator.CrudTest where

import Data.Aeson (KeyValue ((.=)), Value, object)
import Data.Aeson.Types (Pair)
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
    it "returns empty operations list when no operations are defined" $ do
      getCrudOperationJson
        crudOperationsName
        crudWithoutOperations
        primaryEntityField
        `shouldBe` mkOperationsJson []

    it "adds JSON for defined operations" $ do
      getCrudOperationJson
        crudOperationsName
        crudWithoutOperations
          { AS.Crud.operations =
              AS.Crud.CrudOperations
                { get = defaultCrudOperationOptions,
                  getAll = defaultCrudOperationOptions,
                  create = defaultCrudOperationOptions,
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          [ "Get" .= mkOperationJson "get" "tasks/get" NotPublic,
            "GetAll" .= mkOperationJson "get-all" "tasks/get-all" NotPublic,
            "Create" .= mkOperationJson "create" "tasks/create" NotPublic
          ]

    it "returns proper JSON for public operations" $ do
      getCrudOperationJson
        crudOperationsName
        AS.Crud.Crud
          { entity = AS.Core.Ref.Ref crudOperationEntityName,
            operations =
              AS.Crud.CrudOperations
                { get = publicCrudOperationOptions,
                  getAll = privateCrudOperationOptions,
                  create = publicCrudOperationOptions,
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          [ "Get" .= mkOperationJson "get" "tasks/get" Public,
            "GetAll" .= mkOperationJson "get-all" "tasks/get-all" NotPublic,
            "Create" .= mkOperationJson "create" "tasks/create" Public
          ]

    it "allows overrides of operations" $ do
      getCrudOperationJson
        crudOperationsName
        crudWithoutOperations
          { AS.Crud.operations =
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
                  getAll = privateCrudOperationOptions,
                  create = publicCrudOperationOptions,
                  update = Nothing,
                  delete = Nothing
                }
          }
        primaryEntityField
        `shouldBe` mkOperationsJson
          [ "Get" .= mkOperationJson "get" "tasks/get" Public,
            "GetAll" .= mkOperationJson "get-all" "tasks/get-all" NotPublic,
            "Create" .= mkOperationJson "create" "tasks/create" Public
          ]
  where
    crudOperationsName = "tasks"
    crudOperationEntityName = "Task"
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
    crudWithoutOperations =
      AS.Crud.Crud
        { entity = AS.Core.Ref.Ref crudOperationEntityName,
          operations =
            AS.Crud.CrudOperations
              { get = Nothing,
                getAll = Nothing,
                create = Nothing,
                update = Nothing,
                delete = Nothing
              }
        }
    defaultCrudOperationOptions =
      Just
        ( AS.Crud.CrudOperationOptions
            { isPublic = Nothing,
              overrideFn = Nothing
            }
        )
    publicCrudOperationOptions =
      Just
        ( AS.Crud.CrudOperationOptions
            { isPublic = Just True,
              overrideFn = Nothing
            }
        )
    privateCrudOperationOptions =
      Just
        ( AS.Crud.CrudOperationOptions
            { isPublic = Just False,
              overrideFn = Nothing
            }
        )

    mkOperationsJson :: [Pair] -> Value
    mkOperationsJson operations =
      object
        [ "name" .= crudOperationsName,
          "operations" .= object operations,
          "entitiesArray" .= ("['Task']" :: String),
          "primaryFieldName" .= ("id" :: String),
          "entityLower" .= ("task" :: String),
          "entityUpper" .= ("Task" :: String)
        ]

    mkOperationJson :: String -> String -> IsOperationPublic -> Value
    mkOperationJson route fullPath isPublic =
      object
        [ "route" .= route,
          "fullPath" .= fullPath,
          "isPublic" .= case isPublic of
            Public -> True
            NotPublic -> False
        ]

data IsOperationPublic = Public | NotPublic

