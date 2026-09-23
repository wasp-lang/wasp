module Wasp.Generator.DbGenerator.Auth
  ( injectAuth,
    authEntityName,
    authIdentityEntityName,
    sessionEntityName,
    usedOneTimeCodeEntityName,
    oneTimeCodeEntityName,
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

-- | Injects the framework-owned auth entities next to the user entity. The
-- `Session` model exists only when some scheme keeps Wasp-issued credentials
-- in the database (`credentials: { store: "prisma" }`), which is what
-- 'injectSessionEntity' says.
injectAuth :: Bool -> [(String, AS.Entity.Entity)] -> (String, AS.Entity.Entity) -> Generator [(String, AS.Entity.Entity)]
injectAuth injectSessionEntity entities (userEntityName, userEntity) = do
  authEntity <- makeAuthEntity injectSessionEntity userEntityIdField (userEntityName, userEntity)
  authIdentityEntity <- makeAuthIdentityEntity
  sessionEntities <- if injectSessionEntity then (: []) <$> makeSessionEntity else return []
  usedOneTimeCodeEntity <- makeUsedOneTimeCodeEntity
  oneTimeCodeEntity <- makeOneTimeCodeEntity
  let entitiesWithAuth = injectAuthIntoUserEntity userEntityName entities
  return $ entitiesWithAuth ++ [authEntity, authIdentityEntity] ++ sessionEntities ++ [usedOneTimeCodeEntity, oneTimeCodeEntity]
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
          // The name of the auth handler the identity belongs to. The default
          // exists ONLY to backfill rows written before this column did, when
          // Wasp's own auth (named "wasp" unless the app renames it) was the
          // only handler there was. Wasp always writes the column explicitly.
          handlerName    String @default("wasp")
          providerName   String
          providerUserId String

          // What the auth provider asserted about this identity at login (email,
          // name, ...). Written by Wasp when the identity is provisioned and
          // read-only for everyone else, so app code can trust its provenance.
          providerClaims String @default("{}")
          // Non-secret working state the provider keeps for this identity
          // (e.g. isEmailVerified, verification timestamps).
          providerData String @default("{}")
          // Secret material (e.g. the password hash). Omitted from the Prisma
          // client by default so it cannot cross a serialization boundary by
          // accident; auth internals opt back in per query.
          providerSecrets String @default("{}")

          authId    ${authEntityIdTypeText}
          ${authFieldOnAuthIdentityEntityNameText}      ${authEntityNameText} @relation(fields: [authId], references: [id], onDelete: Cascade)

          @@id([handlerName, providerName, providerUserId])
        |]

    authEntityIdTypeText = T.pack authEntityIdType
    authEntityNameText = T.pack authEntityName
    authFieldOnAuthIdentityEntityNameText = T.pack authFieldOnAuthIdentityEntityName

makeAuthEntity :: Bool -> Psl.Model.Field -> (String, AS.Entity.Entity) -> Generator (String, AS.Entity.Entity)
makeAuthEntity withSessions userEntityIdField (userEntityName, _) = case Psl.Parser.Model.parseBody authEntityPslBody of
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
          credentialsInvalidatedAt DateTime?
          ${sessionsRelationText}
        |]

    -- The relation exists only when the Session model does. The
    -- `credentialsInvalidatedAt` stamp always exists: it is how "sign out
    -- everywhere" works for signed-token credentials, which have no rows.
    sessionsRelationText =
      if withSessions
        then sessionsFieldOnAuthEntityNameText <> "   " <> sessionEntityNameText <> "[]"
        else ""

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
          // When the credential was issued, for `user.isCredentialFresh`.
          // The default only backfills rows written before the column did.
          issuedAt  DateTime @default(now())

          // The scheme that verified the login this credential descends from
          // ('wasp', 'clerk', ...), recorded at issue time so that
          // `user.loginScheme` and dual sign-out know which scheme vouched
          // for the login.
          loginScheme String

          // Needs to be called `userId` for Lucia to be able to create sessions
          userId String
          // The relation needs to be named as lowercased entity name, because that's what Lucia expects.
          // If the entity is named `Foo`, the relation needs to be named `foo`.
          ${authFieldOnSessionEntityNameText}   ${authEntityNameText}   @relation(references: [id], fields: [userId], onDelete: Cascade)

          @@index([userId])
        |]

    authEntityNameText = T.pack authEntityName
    authFieldOnSessionEntityNameText = T.pack authFieldOnSessionEntityName

usedOneTimeCodeEntityName :: String
usedOneTimeCodeEntityName = "UsedOneTimeCode"

-- | Replay protection for one-time login codes (the OAuth handback): a code
-- is spent by inserting its row, so two concurrent redemptions are settled by
-- the primary key, whichever server instance they hit -- the previous
-- in-memory store was blind across instances. Rows expire with the code's
-- short JWT lifetime; the store deletes stale ones lazily.
makeUsedOneTimeCodeEntity :: Generator (String, AS.Entity.Entity)
makeUsedOneTimeCodeEntity = case Psl.Parser.Model.parseBody usedOneTimeCodeEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating " ++ usedOneTimeCodeEntityName ++ " entity: " ++ show err
  Right pslBody -> return (usedOneTimeCodeEntityName, AS.Entity.makeEntity pslBody)
  where
    usedOneTimeCodeEntityPslBody =
      T.unpack
        [trimming|
          code   String   @id
          usedAt DateTime @default(now())
        |]

oneTimeCodeEntityName :: String
oneTimeCodeEntityName = "OneTimeCode"

-- | Wasp's one-time codes: a short-lived, single-use stand-in for an account,
-- for a browser navigation that cannot carry a bearer credential
-- (`runtime.createOneTimeCode`). A table of Wasp's own, so it works for
-- every scheme whoever owns the credential. Spending a code is one update
-- guarded by @usedAt@, settled by the database across server instances. No
-- relation to @Auth@: rows live a minute and are removed lazily.
makeOneTimeCodeEntity :: Generator (String, AS.Entity.Entity)
makeOneTimeCodeEntity = case Psl.Parser.Model.parseBody oneTimeCodeEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating " ++ oneTimeCodeEntityName ++ " entity: " ++ show err
  Right pslBody -> return (oneTimeCodeEntityName, AS.Entity.makeEntity pslBody)
  where
    oneTimeCodeEntityPslBody =
      T.unpack
        [trimming|
          code       String    @id
          authId     String
          loginScheme String
          expiresAt  DateTime
          usedAt     DateTime?
        |]

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
