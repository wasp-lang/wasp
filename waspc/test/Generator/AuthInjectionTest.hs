module Generator.AuthInjectionTest where

import Data.Maybe (maybeToList)
import Test.Tasty.Hspec
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.DbGenerator.Auth (injectAuth)
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model

data UserEntityIdField = UserEntityIdField
  { _type :: Psl.Model.FieldType,
    _nativeDbType :: Maybe Psl.Attribute.Attribute
  }

spec_GeneratorAuthInjectionTest :: Spec
spec_GeneratorAuthInjectionTest = do
  describe "injectAuth" $ do
    it "injects auth entities and user entity relation" $ do
      testAuthInjection $
        UserEntityIdField
          { _type = Psl.Model.Int,
            _nativeDbType = Nothing
          }

    it "injects auth entities and user entity relation (user ID is a native db field)" $ do
      testAuthInjection $
        UserEntityIdField
          { _type = Psl.Model.String,
            _nativeDbType = Just $ Psl.Attribute.Attribute "db.Uuid" []
          }
  where
    testAuthInjection :: UserEntityIdField -> Expectation
    testAuthInjection
      UserEntityIdField
        { _type = userEntityIdFieldType,
          _nativeDbType = maybeUserEntityIdFieldNativeDbType
        } = do
        let userEntityIdField =
              Psl.Model.ElementField $
                Psl.Model.Field
                  "id"
                  userEntityIdFieldType
                  []
                  (Psl.Attribute.Attribute "id" [] : maybeToList maybeUserEntityIdFieldNativeDbType)
        let userEntity =
              ( "User",
                AS.Entity.makeEntity $
                  Psl.Model.Body [userEntityIdField]
              )
        let authEntityRelation =
              Psl.Model.ElementField $
                Psl.Model.Field
                  "auth"
                  (Psl.Model.UserType "Auth")
                  [Psl.Model.Optional]
                  []
        let userEntityWithInjectedRelationship =
              ( "User",
                AS.Entity.makeEntity $
                  Psl.Model.Body [userEntityIdField, authEntityRelation]
              )
        let authEntity = makeAuthEntity userEntityIdFieldType maybeUserEntityIdFieldNativeDbType

        let allEntities = [userEntity, someOtherEntity]
        let (_generatorWarnings, generatorResult) = runGenerator $ injectAuth allEntities userEntity
         in generatorResult
              `shouldBe` Right
                [ userEntityWithInjectedRelationship,
                  someOtherEntity,
                  authEntity,
                  authIdentityEntity,
                  sessionEntity
                ]

    makeAuthEntity :: Psl.Model.FieldType -> Maybe Psl.Attribute.Attribute -> (String, AS.Entity.Entity)
    makeAuthEntity userEntityIdFieldType maybeUserEntityIdFieldNativeDbType =
      let userIdField = makeAuthEntityUserIdField userEntityIdFieldType maybeUserEntityIdFieldNativeDbType
       in ( "Auth",
            AS.Entity.makeEntity
              ( Psl.Model.Body
                  [ Psl.Model.ElementField $
                      Psl.Model.Field
                        "id"
                        Psl.Model.String
                        []
                        [ Psl.Attribute.Attribute "id" [],
                          Psl.Attribute.Attribute "default" [Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "uuid" []]
                        ],
                    userIdField,
                    Psl.Model.ElementField $
                      Psl.Model.Field
                        "user"
                        (Psl.Model.UserType "User")
                        [Psl.Model.Optional]
                        [ Psl.Attribute.Attribute
                            "relation"
                            [ Psl.Argument.ArgNamed "fields" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "userId"]),
                              Psl.Argument.ArgNamed "references" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "id"]),
                              Psl.Argument.ArgNamed "onDelete" (Psl.Argument.IdentifierExpr "Cascade")
                            ]
                        ],
                    Psl.Model.ElementField $
                      Psl.Model.Field
                        "identities"
                        (Psl.Model.UserType "AuthIdentity")
                        [Psl.Model.List]
                        [],
                    Psl.Model.ElementField $
                      Psl.Model.Field
                        "sessions"
                        (Psl.Model.UserType "Session")
                        [Psl.Model.List]
                        []
                  ]
              )
          )

    makeAuthEntityUserIdField :: Psl.Model.FieldType -> Maybe Psl.Attribute.Attribute -> Psl.Model.Element
    makeAuthEntityUserIdField userEntityIdFieldType maybeUserEntityIdFieldNativeDbType =
      let userIdFieldAttributes = (Psl.Attribute.Attribute "unique" [] : maybeToList maybeUserEntityIdFieldNativeDbType)
       in Psl.Model.ElementField $
            Psl.Model.Field
              "userId"
              userEntityIdFieldType
              [Psl.Model.Optional]
              userIdFieldAttributes

    authIdentityEntity =
      ( "AuthIdentity",
        AS.Entity.makeEntity
          ( Psl.Model.Body
              [ Psl.Model.ElementField $
                  Psl.Model.Field
                    "providerName"
                    Psl.Model.String
                    []
                    [],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "providerUserId"
                    Psl.Model.String
                    []
                    [],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "providerData"
                    Psl.Model.String
                    []
                    [ Psl.Attribute.Attribute "default" [Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "{}"]
                    ],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "authId"
                    Psl.Model.String
                    []
                    [],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "auth"
                    (Psl.Model.UserType "Auth")
                    []
                    [ Psl.Attribute.Attribute
                        "relation"
                        [ Psl.Argument.ArgNamed "fields" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "authId"]),
                          Psl.Argument.ArgNamed "references" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "id"]),
                          Psl.Argument.ArgNamed "onDelete" (Psl.Argument.IdentifierExpr "Cascade")
                        ]
                    ],
                Psl.Model.ElementBlockAttribute $
                  Psl.Attribute.Attribute "id" [Psl.Argument.ArgUnnamed $ Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "providerName", Psl.Argument.IdentifierExpr "providerUserId"]]
              ]
          )
      )

    sessionEntity =
      ( "Session",
        AS.Entity.makeEntity
          ( Psl.Model.Body
              [ Psl.Model.ElementField $
                  Psl.Model.Field
                    "id"
                    Psl.Model.String
                    []
                    [ Psl.Attribute.Attribute "id" [],
                      Psl.Attribute.Attribute "unique" []
                    ],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "expiresAt"
                    Psl.Model.DateTime
                    []
                    [],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "userId"
                    Psl.Model.String
                    []
                    [],
                Psl.Model.ElementField $
                  Psl.Model.Field
                    "auth"
                    (Psl.Model.UserType "Auth")
                    []
                    [ Psl.Attribute.Attribute
                        "relation"
                        [ Psl.Argument.ArgNamed "references" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "id"]),
                          Psl.Argument.ArgNamed "fields" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "userId"]),
                          Psl.Argument.ArgNamed "onDelete" (Psl.Argument.IdentifierExpr "Cascade")
                        ]
                    ],
                Psl.Model.ElementBlockAttribute $
                  Psl.Attribute.Attribute
                    "index"
                    [ Psl.Argument.ArgUnnamed $ Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "userId"]
                    ]
              ]
          )
      )

    someOtherEntity =
      ( "SomeOtherEntity",
        AS.Entity.makeEntity
          ( Psl.Model.Body
              [ Psl.Model.ElementField $
                  Psl.Model.Field
                    "id"
                    Psl.Model.Int
                    []
                    [ Psl.Attribute.Attribute "id" [],
                      Psl.Attribute.Attribute
                        "default"
                        [ Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "autoincrement" []
                        ]
                    ]
              ]
          )
      )
