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

userFieldOnAuthEntityName :: String
userFieldOnAuthEntityName = "user"

authFieldOnUserEntityName :: String
authFieldOnUserEntityName = "auth"

injectAuth :: Maybe (String, AS.Entity.Entity) -> [(String, AS.Entity.Entity)] -> Generator [(String, AS.Entity.Entity)]
injectAuth Nothing entities = return entities
injectAuth (Just (userEntityName, userEntity)) entities = do
  userEntityIdType <- getUserEntityId userEntity
  authEntity <- makeAuthEntity userEntityIdType userEntityName
  return $ injectAuthIntoUserEntity userEntityName $ entities ++ authEntity

getUserEntityId :: AS.Entity.Entity -> Generator String
getUserEntityId entity =
  show . Psl.Model.Field._type <$> AS.Entity.getIdField entity
    & ( \case
          Nothing -> logAndThrowGeneratorError $ GenericGeneratorError "User entity does not have an id field."
          Just idType -> return idType
      )

makeAuthEntity :: String -> String -> Generator [(String, AS.Entity.Entity)]
makeAuthEntity userEntityIdType userEntityName = case parsePslBody authEntityPslBody of
  Left err -> logAndThrowGeneratorError $ GenericGeneratorError $ "Error while generating Auth entity: " ++ show err
  Right pslBody -> return [(authEntityName, AS.Entity.makeEntity pslBody)]
  where
    -- TODO(miho): decide if we want to switch between fields for username and email
    -- based auth. It's much simpler to just have everything and let some fields be null.
    authEntityPslBody =
      T.unpack
        [trimming|
          id        String   @id @default(uuid())
          email String? @unique
          username String? @unique
          password String?
          isEmailVerified Boolean @default(false)
          emailVerificationSentAt DateTime?
          passwordResetSentAt DateTime?
          userId    ${userEntityIdTypeText}? @unique
          ${userFieldOnAuthEntityNameText}      ${userEntityNameText}?    @relation(fields: [userId], references: [id], onDelete: Cascade)
        |]

    userEntityIdTypeText = T.pack userEntityIdType
    userEntityNameText = T.pack userEntityName
    userFieldOnAuthEntityNameText = T.pack userFieldOnAuthEntityName

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
