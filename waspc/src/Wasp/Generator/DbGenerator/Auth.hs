module Wasp.Generator.DbGenerator.Auth where

import Data.Function ((&))
import Data.Maybe
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

authEntityName :: String
authEntityName = "Auth"

authEntityIdType :: String
authEntityIdType = "String"

userFieldOnAuthEntityName :: String
userFieldOnAuthEntityName = "user"

authFieldOnUserEntityName :: String
authFieldOnUserEntityName = "auth"

authFieldOnProviderEntityName :: String
authFieldOnProviderEntityName = "auth"

providerEntityName :: String
providerEntityName = "SocialAuthProvider"

providersFieldOnAuthEntityName :: String
providersFieldOnAuthEntityName = "providers"

injectAuth :: Maybe (String, AS.Entity.Entity) -> [(String, AS.Entity.Entity)] -> Generator [(String, AS.Entity.Entity)]
injectAuth Nothing entities = return entities
injectAuth (Just (userEntityName, userEntity)) entities = do
  userEntityIdType <- getUserEntityIdType userEntity
  authEntity <- makeAuthEntity userEntityIdType userEntityName userEntity
  providerEntity <- makeProviderEntity
  let entitiesWithAuth = injectAuthIntoUserEntity userEntityName entities
  return $ entitiesWithAuth ++ [authEntity, providerEntity]

getUserEntityIdType :: AS.Entity.Entity -> Generator String
getUserEntityIdType entity =
  show . Psl.Model.Field._type <$> AS.Entity.getIdField entity
    & ( \case
          Nothing -> logAndThrowGeneratorError $ GenericGeneratorError "User entity does not have an id field."
          Just idType -> return idType
      )

makeProviderEntity :: Generator (String, AS.Entity.Entity)
makeProviderEntity = case parsePslBody providerEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating Provider entity: " ++ show err
  Right pslBody -> return (providerEntityName, AS.Entity.makeEntity pslBody)
  where
    providerEntityPslBody =
      T.unpack
        [trimming|
          id         String   @id @default(uuid())
          provider   String
          providerId String
          authId    ${authEntityIdTypeText}
          ${authFieldOnProviderEntityNameText}      ${authEntityNameText} @relation(fields: [authId], references: [id], onDelete: Cascade)
        |]

    authEntityIdTypeText = T.pack authEntityIdType
    authEntityNameText = T.pack authEntityName
    authFieldOnProviderEntityNameText = T.pack authFieldOnProviderEntityName

makeAuthEntity :: String -> String -> AS.Entity.Entity -> Generator (String, AS.Entity.Entity)
makeAuthEntity userEntityIdType userEntityName userEntity = case parsePslBody authEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating Auth entity: " ++ show err
  Right pslBody -> return (authEntityName, AS.Entity.makeEntity pslBody)
  where
    authEntityPslBody =
      T.unpack
        [trimming|
          id ${authEntityIdTypeText}   @id @default(uuid())
          email String? @unique
          username String? @unique
          password String?
          isEmailVerified Boolean @default(false)
          emailVerificationSentAt DateTime?
          passwordResetSentAt DateTime?
          userId    ${userEntityIdTypeText}? @unique
          ${userFieldOnAuthEntityNameText}      ${userEntityNameText}?    @relation(fields: [userId], references: [${userEntityIdFieldName}], onDelete: Cascade)
          ${providersFieldOnAuthEntityNameText} ${providerEntityNameText}[]
        |]

    authEntityIdTypeText = T.pack authEntityIdType
    userEntityIdTypeText = T.pack userEntityIdType
    userEntityNameText = T.pack userEntityName
    userFieldOnAuthEntityNameText = T.pack userFieldOnAuthEntityName
    providerEntityNameText = T.pack providerEntityName
    providersFieldOnAuthEntityNameText = T.pack providersFieldOnAuthEntityName
    userEntityIdFieldName = T.pack $ AS.Entity.getIdField userEntity & fromJust & Psl.Model.Field._name

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
