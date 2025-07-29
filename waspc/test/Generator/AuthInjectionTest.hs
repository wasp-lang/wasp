module Generator.AuthInjectionTest where

import Data.Maybe (maybeToList)
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import Util.Prisma (getPrismaModelBody)
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.DbGenerator.Auth (injectAuth)
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx

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
              Psl.WithCtx.empty $
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
              Psl.WithCtx.empty $
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
              ( Psl.Model.Body $
                  Psl.WithCtx.empty
                    <$> [ Psl.Model.ElementField $
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
      makeEntity
        "AuthIdentity"
        [trimming|
          providerName String
          providerUserId String
          providerData String @default("{}")
          authId String
          auth Auth @relation(fields: [authId], references: [id], onDelete: Cascade)

          @@id([providerName, providerUserId])
        |]

    sessionEntity =
      makeEntity
        "Session"
        [trimming|
          id String @id @unique
          expiresAt DateTime
          userId String
          auth Auth @relation(references: [id], fields: [userId], onDelete: Cascade)

          @@index([userId])
        |]

    someOtherEntity =
      makeEntity
        "SomeOtherEntity"
        [trimming|
          id Int @id @default(autoincrement())
        |]

    makeEntity name bodyText = (name, AS.Entity.makeEntity $ getPrismaModelBody bodyText)
