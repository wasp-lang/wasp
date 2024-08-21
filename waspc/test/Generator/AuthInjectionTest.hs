module Generator.AuthInjectionTest where

import Data.Either (fromRight)
import Test.Tasty.Hspec
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.DbGenerator.Auth (injectAuth)
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model

spec_GeneratorCrudTest :: Spec
spec_GeneratorCrudTest = do
  describe "injectAuth" $ do
    it "injects auth entities and user entity relation" $ do
      let (_generatorWarnings, generatorResult) = runGenerator $ injectAuth [("User", userEntity)] ("User", userEntity)
      let entities = fromRight (error "Auth injection test failed") generatorResult
      entities
        `shouldBe` [ ("User", userEntityWithInjectedRelationship),
                     ( "Auth",
                       makeAuthEntity Psl.Model.Int [Psl.Attribute.Attribute "unique" []]
                     ),
                     ("AuthIdentity", authIdentityEntity),
                     ("Session", sessionEntity)
                   ]

    it "injects auth entities and user entity relation (user ID is a native db fields)" $ do
      let (_, generatorResult) = runGenerator $ injectAuth [("User", userEntityWithNativeIdField)] ("User", userEntityWithNativeIdField)
      let entities = fromRight (error "Auth injection test failed") generatorResult
      entities
        `shouldBe` [ ("User", userEntityWithNativeIdFieldAndInjectedRelationship),
                     ( "Auth",
                       makeAuthEntity
                         Psl.Model.String
                         [ Psl.Attribute.Attribute "unique" [],
                           Psl.Attribute.Attribute "db.Uuid" []
                         ]
                     ),
                     ("AuthIdentity", authIdentityEntity),
                     ("Session", sessionEntity)
                   ]
  where
    userEntity =
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

    userEntityWithInjectedRelationship =
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
                  ],
              Psl.Model.ElementField $
                Psl.Model.Field
                  "auth"
                  (Psl.Model.UserType "Auth")
                  [Psl.Model.Optional]
                  []
            ]
        )

    userEntityWithNativeIdField =
      AS.Entity.makeEntity
        ( Psl.Model.Body
            [ Psl.Model.ElementField $
                Psl.Model.Field
                  "id"
                  Psl.Model.String
                  []
                  [ Psl.Attribute.Attribute "id" [],
                    Psl.Attribute.Attribute "db.Uuid" []
                  ]
            ]
        )

    userEntityWithNativeIdFieldAndInjectedRelationship =
      AS.Entity.makeEntity
        ( Psl.Model.Body
            [ Psl.Model.ElementField $
                Psl.Model.Field
                  "id"
                  Psl.Model.String
                  []
                  [ Psl.Attribute.Attribute "id" [],
                    Psl.Attribute.Attribute "db.Uuid" []
                  ],
              Psl.Model.ElementField $
                Psl.Model.Field
                  "auth"
                  (Psl.Model.UserType "Auth")
                  [Psl.Model.Optional]
                  []
            ]
        )

    makeAuthEntity userIdType userIdAttributes =
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
              Psl.Model.ElementField $
                Psl.Model.Field
                  "userId"
                  userIdType
                  [Psl.Model.Optional]
                  userIdAttributes,
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

    authIdentityEntity =
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

    sessionEntity =
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
