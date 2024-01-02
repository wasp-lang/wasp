module Wasp.Generator.DbGenerator.Auth where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.Analyzer.StdTypeDefinitions.Entity (parsePslBody)
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.Monad
  ( Generator,
    GeneratorError (GenericGeneratorError),
    logAndThrowGeneratorError,
  )
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Model as Psl.Model.Field

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

injectAuth :: [(String, AS.Entity.Entity)] -> (String, AS.Entity.Entity) -> Generator [(String, AS.Entity.Entity)]
injectAuth entities (userEntityName, userEntity) = do
  authEntity <- makeAuthEntity userEntityIdField (userEntityName, userEntity)
  authIdentityEntity <- makeAuthIdentityEntity
  let entitiesWithAuth = injectAuthIntoUserEntity userEntityName entities
  return $ entitiesWithAuth ++ [authEntity, authIdentityEntity]
  where
    -- We validated the AppSpec so we are sure that the user entity has an id field.
    userEntityIdField = fromJust $ AS.Entity.getIdField userEntity

makeAuthIdentityEntity :: Generator (String, AS.Entity.Entity)
makeAuthIdentityEntity = case parsePslBody authIdentityPslBody of
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
makeAuthEntity userEntityIdField (userEntityName, _) = case parsePslBody authEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating Auth entity: " ++ show err
  Right pslBody -> return (authEntityName, AS.Entity.makeEntity pslBody)
  where
    authEntityPslBody =
      T.unpack
        [trimming|
          id ${authEntityIdTypeText}   @id @default(uuid())
          userId    ${userEntityIdTypeText}? @unique
          ${userFieldOnAuthEntityNameText}      ${userEntityNameText}?    @relation(fields: [userId], references: [${userEntityIdFieldName}], onDelete: Cascade)
          ${identitiesFieldOnAuthEntityNameText} ${authIdentityEntityNameText}[]
        |]

    authEntityIdTypeText = T.pack authEntityIdType
    userEntityNameText = T.pack userEntityName
    userFieldOnAuthEntityNameText = T.pack userFieldOnAuthEntityName
    authIdentityEntityNameText = T.pack authIdentityEntityName
    identitiesFieldOnAuthEntityNameText = T.pack identitiesFieldOnAuthEntityName

    userEntityIdTypeText = T.pack $ show . Psl.Model.Field._type $ userEntityIdField
    userEntityIdFieldName = T.pack $ Psl.Model.Field._name userEntityIdField

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
        newPslBody = Psl.Model.Body $ existingPsl ++ relationToAuthEntity
