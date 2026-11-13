module Wasp.Generator.DbGenerator.Mcp
  ( injectMcp,
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.AppSpec.App.Mcp
  ( connectionEntityName,
    mcpConnectionsFieldOnUserEntityName,
    pendingActionEntityName,
    rateLimitEntityName,
  )
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.Monad
  ( Generator,
    GeneratorError (GenericGeneratorError),
    logAndThrowGeneratorError,
  )
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import qualified Wasp.Psl.Generator.Attribute as Psl.Generator.Attribute
import qualified Wasp.Psl.Parser.Model as Psl.Parser.Model

{--

The MCP server (app.mcp) keeps its state in entities Wasp injects into the
Prisma schema, like the auth entities (see DbGenerator.Auth):

User <-> McpConnection (one per authorization a user gave a client) <-> McpPendingAction (destructive calls waiting for approval)
McpRateLimit (one row per connection, keyed by its id)

The runtime library (@wasp/lib-mcp) reads and writes them through Prisma.

--}

-- | Adds the MCP entities and the user entity's relation to connections.
-- Expects the user entity to be in the list (it was validated).
injectMcp :: [AS.Entity.Entity] -> AS.Entity.Entity -> Generator [AS.Entity.Entity]
injectMcp entities userEntity = do
  connectionEntity <- parseEntity connectionEntityName (connectionPslBody userEntity)
  pendingActionEntity <- parseEntity pendingActionEntityName pendingActionPslBody
  rateLimitEntity <- parseEntity rateLimitEntityName rateLimitPslBody
  return $
    map addConnectionsRelation entities
      ++ [connectionEntity, pendingActionEntity, rateLimitEntity]
  where
    addConnectionsRelation entity
      | AS.Entity.getName entity == AS.Entity.getName userEntity = addField connectionsField entity
      | otherwise = entity
    connectionsField =
      Psl.Model.Field
        mcpConnectionsFieldOnUserEntityName
        (Psl.Model.UserType connectionEntityName)
        [Psl.Model.List]
        []

addField :: Psl.Model.Field -> AS.Entity.Entity -> AS.Entity.Entity
addField field entity =
  AS.Entity.makeEntity $
    Psl.Model.Model (AS.Entity.getName entity) (Psl.Model.Body $ existing ++ [Psl.WithCtx.empty $ Psl.Model.ElementField field])
  where
    (Psl.Model.Body existing) = AS.Entity.getPslModelBody entity

parseEntity :: String -> String -> Generator AS.Entity.Entity
parseEntity name body = case Psl.Parser.Model.parseBody body of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating " ++ name ++ " entity: " ++ show err
  Right pslBody -> return $ AS.Entity.makeEntity $ Psl.Model.Model name pslBody

connectionPslBody :: AS.Entity.Entity -> String
connectionPslBody userEntity =
  T.unpack
    [trimming|
      id                  String    @id @default(uuid())
      userId              ${userIdType} ${userIdNativeTypeAttrs}
      user                ${userEntityName} @relation(fields: [userId], references: [${userIdFieldName}], onDelete: Cascade)
      // The client's metadata document URL (its OAuth client_id).
      clientId            String
      clientName          String
      // JSON arrays of scope names.
      scopes              String
      requestedScopes     String
      requireConfirmation Boolean   @default(true)
      // Codes and refresh tokens carry the generation they expect, so each works once.
      refreshGeneration   Int       @default(0)
      createdAt           DateTime  @default(now())
      lastUsedAt          DateTime?
      revokedAt           DateTime?
      ${pendingActionsField} ${pendingActionEntityNameText}[]

      @@index([userId, clientId])
    |]
  where
    -- The user entity was validated to have an id field.
    userIdField = fromJust $ AS.Entity.getIdField userEntity
    userIdType = T.pack $ show $ Psl.Model._type userIdField
    userIdFieldName = T.pack $ Psl.Model._name userIdField
    userIdNativeTypeAttrs =
      T.pack $ unwords $ Psl.Generator.Attribute.generateAttribute <$> filter Psl.Attribute.isNativeDbTypeAttr (Psl.Model._attrs userIdField)
    userEntityName = T.pack $ AS.Entity.getName userEntity
    pendingActionsField = "pendingActions"
    pendingActionEntityNameText = T.pack pendingActionEntityName

pendingActionPslBody :: String
pendingActionPslBody =
  T.unpack
    [trimming|
      id           String        @id @default(uuid())
      connectionId String
      connection   ${connectionEntityNameText} @relation(fields: [connectionId], references: [id], onDelete: Cascade)
      toolName     String
      // JSON
      args         String
      // A hash of the args' canonical JSON, to find an identical earlier call.
      argsKey      String
      // JSON: { title, details }
      summary      String
      // JSON: the result or the error
      outcome      String?
      expiresAt    DateTime
      approvedAt   DateTime?
      deniedAt     DateTime?
      reportedAt   DateTime?
      createdAt    DateTime      @default(now())

      @@index([connectionId, toolName, argsKey])
    |]
  where
    connectionEntityNameText = T.pack connectionEntityName

rateLimitPslBody :: String
rateLimitPslBody =
  T.unpack
    [trimming|
      // The connection's id.
      id          String   @id
      windowStart DateTime
      count       Int
    |]
