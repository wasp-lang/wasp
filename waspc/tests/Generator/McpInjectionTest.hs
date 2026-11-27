module Generator.McpInjectionTest where

import Data.List (find)
import Test.Hspec
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.DbGenerator.Mcp (injectMcp)
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx

spec_GeneratorMcpInjectionTest :: Spec
spec_GeneratorMcpInjectionTest =
  describe "injectMcp" $ do
    it "adds the MCP entities and the user entity's relation to connections" $ do
      let entities = inject Psl.Model.Int []
      map AS.Entity.getName entities
        `shouldBe` ["User", "Task", "McpConnection", "McpPendingAction", "McpRateLimit"]
      fieldNames (entityNamed "User" entities) `shouldBe` ["id", "mcpConnections"]
      fieldNames (entityNamed "McpConnection" entities)
        `shouldBe` [ "id",
                     "userId",
                     "user",
                     "clientId",
                     "clientName",
                     "scopes",
                     "requestedScopes",
                     "requireConfirmation",
                     "refreshGeneration",
                     "createdAt",
                     "lastUsedAt",
                     "revokedAt",
                     "pendingActions"
                   ]

    it "gives connections a userId of the user entity's id type, with its native db type" $ do
      let userIdOf = fieldNamed "userId" . entityNamed "McpConnection"
      let uuidUserId = userIdOf $ inject Psl.Model.String [Psl.Attribute.Attribute "db.Uuid" []]
      Psl.Model._type uuidUserId `shouldBe` Psl.Model.String
      map Psl.Attribute._attrName (Psl.Model._attrs uuidUserId) `shouldBe` ["db.Uuid"]
      Psl.Model._type (userIdOf $ inject Psl.Model.BigInt []) `shouldBe` Psl.Model.BigInt
  where
    inject idType nativeAttrs =
      let idField = Psl.Model.Field "id" idType [] (Psl.Attribute.Attribute "id" [] : nativeAttrs)
          userEntity = AS.Entity.makeEntity $ Psl.Model.Model "User" $ Psl.Model.Body [Psl.WithCtx.empty $ Psl.Model.ElementField idField]
          taskEntity = AS.Entity.makeEntity $ Psl.Model.Model "Task" $ Psl.Model.Body []
       in case snd $ runGenerator $ injectMcp [userEntity, taskEntity] userEntity of
            Right entities -> entities
            Left errors -> error (show errors)
    entityNamed name entities = case find ((== name) . AS.Entity.getName) entities of
      Just entity -> entity
      Nothing -> error ("No entity " ++ name)
    fieldNames = map Psl.Model._name . AS.Entity.getFields
    fieldNamed name entity = case find ((== name) . Psl.Model._name) (AS.Entity.getFields entity) of
      Just field -> field
      Nothing -> error ("No field " ++ name)
