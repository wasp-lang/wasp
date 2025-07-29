module Wasp.Generator.DbGenerator.Auth
  ( injectAuth,
    authEntityName,
    authIdentityEntityName,
    sessionEntityName,
    userFieldOnAuthEntityName,
    authFieldOnUserEntityName,
    identitiesFieldOnAuthEntityName,
    authFieldOnAuthIdentityEntityName,
  )
where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import NeatInterpolation (trimming)
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
import qualified Wasp.Util as Util

{--

A bit of explanation on how auth works in Wasp:

If developers want to use Wasp's auth, they have to define the user entity.
Their user entity represents the buisness logic user. Which means, it doesn't need to have
any auth related fields e.g. email, password, etc. The user entity can have any fields that
the developer wants.

Wasp injects extra entities into the Prisma schema: Auth and AuthIdentity which are connected
to the business logic entity. All of the internal auth logic is done on Auth and AuthIdentity
entities.

The developer doesn't have to worry about these entities, they are hidden from the developer.
The developer can still use the user entity as they would normally. The developer can also
use the Auth and AuthIdentity entities if they require custom auth logic.

User <-> Auth (one on one) <-> AuthIdentity (per provider)

The AuthIdentity entity is used to store the user's identity from a specific provider
(e.g. Google, email, etc.). The Auth entity is used to have a single
connection between the business logic user and the auth identities.

--}

authEntityName :: String
authEntityName = "Auth"

authEntityIdType :: String
authEntityIdType = "String"

userFieldOnAuthEntityName :: String
userFieldOnAuthEntityName = "user"

authFieldOnUserEntityName :: String
authFieldOnUserEntityName = "auth"

authFieldOnAuthIdentityEntityName :: String
authFieldOnAuthIdentityEntityName = "auth"

authIdentityEntityName :: String
authIdentityEntityName = "AuthIdentity"

identitiesFieldOnAuthEntityName :: String
identitiesFieldOnAuthEntityName = "identities"

sessionEntityName :: String
sessionEntityName = "Session"

sessionsFieldOnAuthEntityName :: String
sessionsFieldOnAuthEntityName = "sessions"

authFieldOnSessionEntityName :: String
authFieldOnSessionEntityName = Util.toLowerFirst authEntityName

injectAuth :: [(String, AS.Entity.Entity)] -> (String, AS.Entity.Entity) -> Generator [(String, AS.Entity.Entity)]
injectAuth entities (userEntityName, userEntity) = do
  authEntity <- makeAuthEntity userEntityIdField (userEntityName, userEntity)
  authIdentityEntity <- makeAuthIdentityEntity
  sessionEntity <- makeSessionEntity
  let entitiesWithAuth = injectAuthIntoUserEntity userEntityName entities
  return $ entitiesWithAuth ++ [authEntity, authIdentityEntity, sessionEntity]
  where
    -- We validated the AppSpec so we are sure that the user entity has an id field.
    userEntityIdField = fromJust $ AS.Entity.getIdField userEntity

makeAuthIdentityEntity :: Generator (String, AS.Entity.Entity)
makeAuthIdentityEntity = case Psl.Parser.Model.parseBody authIdentityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating " ++ authIdentityEntityName ++ " entity: " ++ show err
  Right pslBody -> return (authIdentityEntityName, AS.Entity.makeEntity pslBody)
  where
    authIdentityPslBody =
      T.unpack
        [trimming|
          providerName   String
          providerUserId String

          providerData String @default("{}")

          authId    ${authEntityIdTypeText}
          ${authFieldOnAuthIdentityEntityNameText}      ${authEntityNameText} @relation(fields: [authId], references: [id], onDelete: Cascade)

          @@id([providerName, providerUserId])
        |]

    authEntityIdTypeText = T.pack authEntityIdType
    authEntityNameText = T.pack authEntityName
    authFieldOnAuthIdentityEntityNameText = T.pack authFieldOnAuthIdentityEntityName

makeAuthEntity :: Psl.Model.Field -> (String, AS.Entity.Entity) -> Generator (String, AS.Entity.Entity)
makeAuthEntity userEntityIdField (userEntityName, _) = case Psl.Parser.Model.parseBody authEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating " ++ authEntityName ++ " entity: " ++ show err
  Right pslBody -> return (authEntityName, AS.Entity.makeEntity pslBody)
  where
    authEntityPslBody =
      T.unpack
        [trimming|
          id ${authEntityIdTypeText}   @id @default(uuid())
          userId    ${userEntityIdTypeText}? ${userEntityIdFieldAttributesText}
          ${userFieldOnAuthEntityNameText}      ${userEntityNameText}?    @relation(fields: [userId], references: [${userEntityIdFieldName}], onDelete: Cascade)
          ${identitiesFieldOnAuthEntityNameText} ${authIdentityEntityNameText}[]
          ${sessionsFieldOnAuthEntityNameText}   ${sessionEntityNameText}[]
        |]

    authEntityIdTypeText = T.pack authEntityIdType
    userEntityNameText = T.pack userEntityName
    userFieldOnAuthEntityNameText = T.pack userFieldOnAuthEntityName
    authIdentityEntityNameText = T.pack authIdentityEntityName
    identitiesFieldOnAuthEntityNameText = T.pack identitiesFieldOnAuthEntityName
    sessionsFieldOnAuthEntityNameText = T.pack sessionsFieldOnAuthEntityName
    sessionEntityNameText = T.pack sessionEntityName

    userEntityIdTypeText = T.pack $ show . Psl.Model._type $ userEntityIdField
    userEntityIdFieldName = T.pack $ Psl.Model._name userEntityIdField
    userEntityIdFieldAttributesText = T.pack $ makeUserEntityIdFieldAttributes userEntityIdField

makeUserEntityIdFieldAttributes :: Psl.Model.Field -> String
makeUserEntityIdFieldAttributes field = unwords attrs
  where
    attrs = waspDefinedAttrs ++ (Psl.Generator.Attribute.generateAttribute <$> userDefinedNativeDbTypeAttributes)
    waspDefinedAttrs = ["@unique"]
    userDefinedNativeDbTypeAttributes = filter Psl.Attribute.isNativeDbTypeAttr $ Psl.Model._attrs field

makeSessionEntity :: Generator (String, AS.Entity.Entity)
makeSessionEntity = case Psl.Parser.Model.parseBody sessionEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating " ++ sessionEntityName ++ " entity: " ++ show err
  Right pslBody -> return (sessionEntityName, AS.Entity.makeEntity pslBody)
  where
    sessionEntityPslBody =
      T.unpack
        [trimming|
          id        String   @id @unique
          expiresAt DateTime

          // Needs to be called `userId` for Lucia to be able to create sessions
          userId String
          // The relation needs to be named as lowercased entity name, because that's what Lucia expects.
          // If the entity is named `Foo`, the relation needs to be named `foo`.
          ${authFieldOnSessionEntityNameText}   ${authEntityNameText}   @relation(references: [id], fields: [userId], onDelete: Cascade)

          @@index([userId])
        |]

    authEntityNameText = T.pack authEntityName
    authFieldOnSessionEntityNameText = T.pack authFieldOnSessionEntityName

injectAuthIntoUserEntity :: String -> [(String, AS.Entity.Entity)] -> [(String, AS.Entity.Entity)]
injectAuthIntoUserEntity userEntityName entities =
  let userEntity = fromJust $ lookup userEntityName entities
      userEntityWithAuthInjected = injectRelationToAuth userEntity
   in (userEntityName, userEntityWithAuthInjected) : filter ((/= userEntityName) . fst) entities
  where
    injectRelationToAuth :: AS.Entity.Entity -> AS.Entity.Entity
    injectRelationToAuth entity = AS.Entity.makeEntity newPslBody
      where
        (Psl.Model.Body existingPsl) = AS.Entity.getPslModelBody entity
        relationToAuthEntity =
          [ Psl.Model.ElementField $
              Psl.Model.Field
                authFieldOnUserEntityName
                (Psl.Model.UserType authEntityName)
                [Psl.Model.Optional]
                []
          ]
        newPslBody = Psl.Model.Body $ existingPsl ++ (Psl.WithCtx.empty <$> relationToAuthEntity)
