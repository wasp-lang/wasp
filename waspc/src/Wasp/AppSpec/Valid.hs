{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Valid
  ( validateAppSpec,
    ValidationError (..),
    getApp,
    isAuthEnabled,
    doesUserEntityContainField,
    getIdFieldFromCrudEntity,
  )
where

import Control.Monad (unless)
import Data.List (find, group, groupBy, intercalate, sort, sortBy)
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.ApiNamespace as AS.ApiNamespace
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import qualified Wasp.AppSpec.App.Client as Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Wasp as Wasp
import Wasp.AppSpec.Core.Decl (takeDecls)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Entity (isFieldUnique)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.AppSpec.Entity.Field as Entity.Field
import qualified Wasp.AppSpec.Page as Page
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.Generator.Crud (crudDeclarationToOperationsList)
import qualified Wasp.Psl.Ast.Model as PslModel
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Version as WV

data ValidationError = GenericValidationError String
  deriving (Eq)

instance Show ValidationError where
  show (GenericValidationError e) = e

validateAppSpec :: AppSpec -> [ValidationError]
validateAppSpec spec =
  case validateExactlyOneAppExists spec of
    Just err -> [err]
    Nothing ->
      -- NOTE: We check these only if App exists because they all rely on it existing.
      concat
        [ validateWasp spec,
          validateAppAuthIsSetIfAnyPageRequiresAuth spec,
          validateOnlyEmailOrUsernameAndPasswordAuthIsUsed spec,
          validateAuthUserEntityHasCorrectFieldsIfUsernameAndPasswordAuthIsUsed spec,
          validateAuthUserEntityHasCorrectFieldsIfEmailAuthIsUsed spec,
          validateEmailSenderIsDefinedIfEmailAuthIsUsed spec,
          validateExternalAuthEntityHasCorrectFieldsIfExternalAuthIsUsed spec,
          validateDbIsPostgresIfPgBossUsed spec,
          validateApiRoutesAreUnique spec,
          validateApiNamespacePathsAreUnique spec,
          validateCrudOperations spec,
          validatePrismaOptions spec,
          validateWebAppBaseDir spec
        ]

validateExactlyOneAppExists :: AppSpec -> Maybe ValidationError
validateExactlyOneAppExists spec =
  case AS.takeDecls @App (AS.decls spec) of
    [] -> Just $ GenericValidationError "You are missing an 'app' declaration in your Wasp app."
    [_] -> Nothing
    apps ->
      Just $
        GenericValidationError $
          "You have more than one 'app' declaration in your Wasp app. You have " ++ show (length apps) ++ "."

validateWasp :: AppSpec -> [ValidationError]
validateWasp = validateWaspVersion . Wasp.version . App.wasp . snd . getApp

validateWaspVersion :: String -> [ValidationError]
validateWaspVersion specWaspVersionStr = eitherUnitToErrorList $ do
  specWaspVersionRange <- parseWaspVersionRange specWaspVersionStr
  unless (SV.isVersionInRange WV.waspVersion specWaspVersionRange) $
    Left $
      incompatibleVersionError WV.waspVersion specWaspVersionRange
  where
    -- TODO: Use version range parser from SemanticVersion when it is fully implemented.

    parseWaspVersionRange :: String -> Either ValidationError SV.Range
    parseWaspVersionRange waspVersionRangeStr = do
      -- Only ^x.y.z is allowed here because it was the easiest solution to start
      -- with at the moment. In the future, we plan to allow any SemVer
      -- definition.
      let (_ :: String, _ :: String, _ :: String, waspVersionRangeDigits :: [String]) =
            waspVersionRangeStr =~ ("\\`\\^([0-9]+)\\.([0-9]+)\\.([0-9]+)\\'" :: String)

      waspSpecVersion <- case mapM readMaybe waspVersionRangeDigits of
        Just [major, minor, patch] -> Right $ SV.Version major minor patch
        __ -> Left $ GenericValidationError "Wasp version should be in the format ^major.minor.patch"

      Right $ SV.Range [SV.backwardsCompatibleWith waspSpecVersion]

    incompatibleVersionError :: SV.Version -> SV.Range -> ValidationError
    incompatibleVersionError actualVersion expectedVersionRange =
      GenericValidationError $
        unlines
          [ "Your Wasp version does not match the app's requirements.",
            "You are running Wasp " ++ show actualVersion ++ ".",
            "This app requires Wasp " ++ show expectedVersionRange ++ ".",
            "To install specific version of Wasp, do:",
            "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v x.y.z",
            "where x.y.z is your desired version.",
            "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions."
          ]

    eitherUnitToErrorList :: Either e () -> [e]
    eitherUnitToErrorList (Left e) = [e]
    eitherUnitToErrorList (Right ()) = []

validateAppAuthIsSetIfAnyPageRequiresAuth :: AppSpec -> [ValidationError]
validateAppAuthIsSetIfAnyPageRequiresAuth spec =
  [ GenericValidationError
      "Expected app.auth to be defined since there are Pages with authRequired set to true."
    | anyPageRequiresAuth && not (isAuthEnabled spec)
  ]
  where
    anyPageRequiresAuth = any ((== Just True) . Page.authRequired) (snd <$> AS.getPages spec)

validateOnlyEmailOrUsernameAndPasswordAuthIsUsed :: AppSpec -> [ValidationError]
validateOnlyEmailOrUsernameAndPasswordAuthIsUsed spec =
  case App.auth (snd $ getApp spec) of
    Nothing -> []
    Just auth ->
      [ GenericValidationError
          "Expected app.auth to use either email or username and password authentication, but not both."
        | areBothAuthMethodsUsed
      ]
      where
        areBothAuthMethodsUsed = Auth.isEmailAuthEnabled auth && Auth.isUsernameAndPasswordAuthEnabled auth

validateDbIsPostgresIfPgBossUsed :: AppSpec -> [ValidationError]
validateDbIsPostgresIfPgBossUsed spec =
  [ GenericValidationError
      "Expected app.db.system to be PostgreSQL since there are jobs with executor set to PgBoss."
    | isPgBossJobExecutorUsed spec && not (isPostgresUsed spec)
  ]

validateAuthUserEntityHasCorrectFieldsIfUsernameAndPasswordAuthIsUsed :: AppSpec -> [ValidationError]
validateAuthUserEntityHasCorrectFieldsIfUsernameAndPasswordAuthIsUsed spec = case App.auth (snd $ getApp spec) of
  Nothing -> []
  Just auth ->
    if not $ Auth.isUsernameAndPasswordAuthEnabled auth
      then []
      else validationErrors
    where
      validationErrors = concat [usernameValidationErrors, passwordValidationErrors]
      usernameValidationErrors
        | not $ null usernameTypeValidationErrors = usernameTypeValidationErrors
        | otherwise = usernameAttributeValidationErrors
      passwordValidationErrors =
        validateEntityHasField
          userEntityName
          authUserEntityPath
          userEntityFields
          ("password", Entity.Field.FieldTypeScalar Entity.Field.String, "String")
      usernameTypeValidationErrors =
        validateEntityHasField
          userEntityName
          authUserEntityPath
          userEntityFields
          ("username", Entity.Field.FieldTypeScalar Entity.Field.String, "String")
      usernameAttributeValidationErrors
        | isFieldUnique "username" userEntity == Just True = []
        | otherwise =
            [ GenericValidationError $
                "The field 'username' on entity '"
                  ++ userEntityName
                  ++ "' (referenced by "
                  ++ authUserEntityPath
                  ++ ") must be marked with the '@unique' attribute."
            ]
      userEntityFields = Entity.getFields userEntity
      authUserEntityPath = "app.auth.userEntity"
      (userEntityName, userEntity) = AS.resolveRef spec (Auth.userEntity auth)

validateAuthUserEntityHasCorrectFieldsIfEmailAuthIsUsed :: AppSpec -> [ValidationError]
validateAuthUserEntityHasCorrectFieldsIfEmailAuthIsUsed spec = case App.auth (snd $ getApp spec) of
  Nothing -> []
  Just auth ->
    if not $ Auth.isEmailAuthEnabled auth
      then []
      else
        let (userEntityName, userEntity) = AS.resolveRef spec (Auth.userEntity auth)
            userEntityFields = Entity.getFields userEntity
         in concatMap
              (validateEntityHasField userEntityName "app.auth.userEntity" userEntityFields)
              [ ("email", Entity.Field.FieldTypeComposite (Entity.Field.Optional Entity.Field.String), "String"),
                ("password", Entity.Field.FieldTypeComposite (Entity.Field.Optional Entity.Field.String), "String"),
                ("isEmailVerified", Entity.Field.FieldTypeScalar Entity.Field.Boolean, "Boolean"),
                ("emailVerificationSentAt", Entity.Field.FieldTypeComposite (Entity.Field.Optional Entity.Field.DateTime), "DateTime?"),
                ("passwordResetSentAt", Entity.Field.FieldTypeComposite (Entity.Field.Optional Entity.Field.DateTime), "DateTime?")
              ]

validateEmailSenderIsDefinedIfEmailAuthIsUsed :: AppSpec -> [ValidationError]
validateEmailSenderIsDefinedIfEmailAuthIsUsed spec = case App.auth app of
  Nothing -> []
  Just auth ->
    if not $ Auth.isEmailAuthEnabled auth
      then []
      else case App.emailSender app of
        Nothing -> [GenericValidationError "app.emailSender must be specified when using email auth."]
        Just _ -> []
  where
    app = snd $ getApp spec

validateExternalAuthEntityHasCorrectFieldsIfExternalAuthIsUsed :: AppSpec -> [ValidationError]
validateExternalAuthEntityHasCorrectFieldsIfExternalAuthIsUsed spec = case App.auth (snd $ getApp spec) of
  Nothing -> []
  Just auth ->
    if not $ Auth.isExternalAuthEnabled auth
      then []
      else case Auth.externalAuthEntity auth of
        Nothing -> [GenericValidationError "app.auth.externalAuthEntity must be specified when using a social login method."]
        Just externalAuthEntityRef ->
          let (userEntityName, userEntity) = AS.resolveRef spec (Auth.userEntity auth)
              userEntityFields = Entity.getFields userEntity
              (externalAuthEntityName, externalAuthEntity) = AS.resolveRef spec externalAuthEntityRef
              externalAuthEntityFields = Entity.getFields externalAuthEntity
              externalAuthEntityValidationErrors =
                concatMap
                  (validateEntityHasField externalAuthEntityName "app.auth.externalAuthEntity" externalAuthEntityFields)
                  [ ("provider", Entity.Field.FieldTypeScalar Entity.Field.String, "String"),
                    ("providerId", Entity.Field.FieldTypeScalar Entity.Field.String, "String"),
                    ("user", Entity.Field.FieldTypeScalar (Entity.Field.UserType userEntityName), userEntityName),
                    ("userId", Entity.Field.FieldTypeScalar Entity.Field.Int, "Int")
                  ]
              userEntityValidationErrors =
                concatMap
                  (validateEntityHasField userEntityName "app.auth.userEntity" userEntityFields)
                  [ ( "externalAuthAssociations",
                      Entity.Field.FieldTypeComposite $ Entity.Field.List $ Entity.Field.UserType externalAuthEntityName,
                      externalAuthEntityName ++ "[]"
                    )
                  ]
           in externalAuthEntityValidationErrors ++ userEntityValidationErrors

validateEntityHasField :: String -> String -> [Entity.Field.Field] -> (String, Entity.Field.FieldType, String) -> [ValidationError]
validateEntityHasField entityName authEntityPath entityFields (fieldName, fieldType, fieldTypeName) =
  let maybeField = findFieldByName fieldName entityFields
   in case maybeField of
        Just providerField
          | Entity.Field.fieldType providerField == fieldType -> []
        _ ->
          [ GenericValidationError $
              "Entity '" ++ entityName ++ "' (referenced by " ++ authEntityPath ++ ") must have field '" ++ fieldName ++ "' of type '" ++ fieldTypeName ++ "'."
          ]

validateApiRoutesAreUnique :: AppSpec -> [ValidationError]
validateApiRoutesAreUnique spec =
  if null groupsOfConflictingRoutes
    then []
    else [GenericValidationError $ "`api` routes must be unique. Duplicates: " ++ intercalate ", " (show <$> groupsOfConflictingRoutes)]
  where
    apiRoutes = AS.Api.httpRoute . snd <$> AS.getApis spec
    groupsOfConflictingRoutes = filter ((> 1) . length) (groupBy routesHaveConflictingDefinitions $ sortBy routeComparator apiRoutes)

    routeComparator :: (AS.Api.HttpMethod, String) -> (AS.Api.HttpMethod, String) -> Ordering
    routeComparator l r | routesHaveConflictingDefinitions l r = EQ
    routeComparator l r = compare l r

    -- Two routes have conflicting definitions if they define the same thing twice,
    -- so we don't know which definition to use. This can happen if they are exactly
    -- the same (path and method) or if they have the same paths and one has ALL for a method.
    routesHaveConflictingDefinitions :: (AS.Api.HttpMethod, String) -> (AS.Api.HttpMethod, String) -> Bool
    routesHaveConflictingDefinitions (lMethod, lPath) (rMethod, rPath) =
      lPath == rPath && (lMethod == rMethod || AS.Api.ALL `elem` [lMethod, rMethod])

validateApiNamespacePathsAreUnique :: AppSpec -> [ValidationError]
validateApiNamespacePathsAreUnique spec =
  if null duplicatePaths
    then []
    else [GenericValidationError $ "`apiNamespace` paths must be unique. Duplicates: " ++ intercalate ", " duplicatePaths]
  where
    namespacePaths = AS.ApiNamespace.path . snd <$> AS.getApiNamespaces spec
    duplicatePaths = map head $ filter ((> 1) . length) (group . sort $ namespacePaths)

validateCrudOperations :: AppSpec -> [ValidationError]
validateCrudOperations spec =
  concat
    [ concatMap checkIfAtLeastOneOperationIsUsedForCrud cruds,
      concatMap checkIfSimpleIdFieldIsDefinedForEntity cruds
    ]
  where
    cruds = AS.getCruds spec

    checkIfAtLeastOneOperationIsUsedForCrud :: (String, AS.Crud.Crud) -> [ValidationError]
    checkIfAtLeastOneOperationIsUsedForCrud (crudName, crud) =
      if not . null $ crudOperations
        then []
        else [GenericValidationError $ "CRUD \"" ++ crudName ++ "\" must have at least one operation defined."]
      where
        crudOperations = crudDeclarationToOperationsList crud

    checkIfSimpleIdFieldIsDefinedForEntity :: (String, AS.Crud.Crud) -> [ValidationError]
    checkIfSimpleIdFieldIsDefinedForEntity (crudName, crud) = case (maybeIdField, maybeIdBlockAttribute) of
      (Just _, Nothing) -> []
      (Nothing, Just _) ->
        [ GenericValidationError $
            "Entity '"
              ++ entityName
              ++ "' (referenced by CRUD declaration '"
              ++ crudName
              ++ "') must have an ID field (specified with the '@id' attribute) and not a composite ID (specified with the '@@id' attribute)."
        ]
      _missingIdFieldWithoutBlockIdAttributeDefined ->
        [ GenericValidationError $
            "Entity '"
              ++ entityName
              ++ "' (referenced by CRUD declaration '"
              ++ crudName
              ++ "') must have an ID field (specified with the '@id' attribute)."
        ]
      where
        maybeIdField = Entity.getIdField entity
        maybeIdBlockAttribute = Entity.getIdBlockAttribute entity
        (entityName, entity) = AS.resolveRef spec (AS.Crud.entity crud)

validatePrismaOptions :: AppSpec -> [ValidationError]
validatePrismaOptions spec =
  concat
    [ checkIfPostgresExtensionsAreUsedWithoutPostgresDbSystem,
      checkIfDbExtensionsAreUsedWithoutPostgresDbSystem,
      checkIfDbExtensionsAreUsedWithoutPostgresPreviewFlag
    ]
  where
    checkIfPostgresExtensionsAreUsedWithoutPostgresDbSystem :: [ValidationError]
    checkIfPostgresExtensionsAreUsedWithoutPostgresDbSystem = maybe [] check prismaClientPreviewFeatures
      where
        check :: [String] -> [ValidationError]
        check previewFeatures =
          if not isPostgresDbUsed && "postgresqlExtensions" `elem` previewFeatures
            then [GenericValidationError "You enabled \"postgresqlExtensions\" in app.db.prisma.clientPreviewFeatures but your db system is not PostgreSQL."]
            else []

    checkIfDbExtensionsAreUsedWithoutPostgresDbSystem :: [ValidationError]
    checkIfDbExtensionsAreUsedWithoutPostgresDbSystem = maybe [] check prismaDbExtensions
      where
        check :: [AS.Db.PrismaDbExtension] -> [ValidationError]
        check value =
          if not isPostgresDbUsed && not (null value)
            then [GenericValidationError "If you are using app.db.prisma.dbExtensions you must use PostgreSQL as your db system."]
            else []

    checkIfDbExtensionsAreUsedWithoutPostgresPreviewFlag :: [ValidationError]
    checkIfDbExtensionsAreUsedWithoutPostgresPreviewFlag = case (prismaDbExtensions, prismaClientPreviewFeatures) of
      (Nothing, _) -> []
      (Just _extensions, Just features) | "postgresqlExtensions" `elem` features -> []
      (Just _extensions, _) -> [GenericValidationError extensionsNotEnabledMessage]
      where
        extensionsNotEnabledMessage = "You are using app.db.prisma.dbExtensions but you didn't enable \"postgresqlExtensions\" in app.db.prisma.clientPreviewFeatures."

    isPostgresDbUsed = isPostgresUsed spec
    prismaOptions = AS.Db.prisma =<< AS.App.db (snd $ getApp spec)
    prismaClientPreviewFeatures = AS.Db.clientPreviewFeatures =<< prismaOptions
    prismaDbExtensions = AS.Db.dbExtensions =<< prismaOptions

validateWebAppBaseDir :: AppSpec -> [ValidationError]
validateWebAppBaseDir spec = case maybeBaseDir of
  Just baseDir
    | not (startsWithSlash baseDir) ->
        [GenericValidationError "The app.client.baseDir should start with a slash e.g. \"/test\""]
  _anyOtherCase -> []
  where
    maybeBaseDir = Client.baseDir =<< AS.App.client (snd $ getApp spec)

    startsWithSlash :: String -> Bool
    startsWithSlash ('/' : _) = True
    startsWithSlash _ = False

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
-- TODO: It would be great if we could ensure this at type level, but we decided that was too much work for now.
--   Check https://github.com/wasp-lang/wasp/pull/455 for considerations on this and analysis of different approaches.
getApp :: AppSpec -> (String, App)
getApp spec = case takeDecls @App (AS.decls spec) of
  [app] -> app
  apps ->
    error $
      ("Expected exactly 1 'app' declaration in your wasp code, but you have " ++ show (length apps) ++ ".")
        ++ " This should never happen as it should have been caught during validation of AppSpec."

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
isAuthEnabled :: AppSpec -> Bool
isAuthEnabled spec = isJust (App.auth $ snd $ getApp spec)

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
getDbSystem :: AppSpec -> Maybe AS.Db.DbSystem
getDbSystem spec = AS.Db.system =<< AS.App.db (snd $ getApp spec)

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
isPostgresUsed :: AppSpec -> Bool
isPostgresUsed = (Just AS.Db.PostgreSQL ==) . getDbSystem

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
-- If there is no user entity, it returns Nothing.
doesUserEntityContainField :: AppSpec -> String -> Maybe Bool
doesUserEntityContainField spec fieldName = do
  auth <- App.auth (snd $ getApp spec)
  let userEntity = snd $ AS.resolveRef spec (Auth.userEntity auth)
  let userEntityFields = Entity.getFields userEntity
  Just $ isJust $ findFieldByName fieldName userEntityFields

findFieldByName :: String -> [Entity.Field.Field] -> Maybe Entity.Field.Field
findFieldByName name = find ((== name) . Entity.Field.fieldName)

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
-- We validated that entity field exists, so we can safely use fromJust here.
getIdFieldFromCrudEntity :: AppSpec -> AS.Crud.Crud -> PslModel.Field
getIdFieldFromCrudEntity spec crud = fromJust $ Entity.getIdField crudEntity
  where
    crudEntity = snd $ AS.resolveRef spec (AS.Crud.entity crud)
