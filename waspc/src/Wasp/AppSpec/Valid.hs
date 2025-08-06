{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Valid
  ( validateAppSpec,
    getApp,
    isAuthEnabled,
    doesUserEntityContainField,
    getIdFieldFromCrudEntity,
    getLowestNodeVersionUserAllows,
    getValidDbSystem,
  )
where

import Control.Monad (unless)
import Data.List (find, group, groupBy, intercalate, sort, sortBy)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
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
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.AppSpec.App.Wasp as Wasp
import Wasp.AppSpec.Core.Decl (takeDecls)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.AppSpec.Entity.Field as Entity.Field
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Page as Page
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.Generator.Crud (crudDeclarationToOperationsList)
import Wasp.Node.Version (oldestWaspSupportedNodeVersion)
import qualified Wasp.Node.Version as V
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Db as Psl.Db
import qualified Wasp.Psl.Util as Psl.Util
import Wasp.Psl.Valid (getValidDbSystemFromPrismaSchema)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.SemanticVersion.VersionBound as SVB
import Wasp.Util (findDuplicateElems, isCapitalized)
import Wasp.Valid (ValidationError (..))
import qualified Wasp.Version as WV

validateAppSpec :: AppSpec -> [ValidationError]
validateAppSpec spec =
  case validateExactlyOneAppExists spec of
    Just err -> [err]
    Nothing ->
      -- NOTE: We check these only if App exists because they all rely on it existing.
      concat
        [ validateWasp spec,
          validateAppAuthIsSetIfAnyPageRequiresAuth spec,
          validateUserEntity spec,
          validateOnlyEmailOrUsernameAndPasswordAuthIsUsed spec,
          validateEmailSenderIsDefinedIfEmailAuthIsUsed spec,
          validateDummyEmailSenderIsNotUsedInProduction spec,
          validateDbIsPostgresIfPgBossUsed spec,
          validateApiRoutesAreUnique spec,
          validateApiNamespacePathsAreUnique spec,
          validateCrudOperations spec,
          validateUniqueDeclarationNames spec,
          validateDeclarationNames spec,
          validateWebAppBaseDir spec,
          validateUserNodeVersionRange spec,
          validateAtLeastOneRoute spec
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
            "  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z",
            "where x.y.z is your desired version.",
            "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions."
          ]

    eitherUnitToErrorList :: Either e () -> [e]
    eitherUnitToErrorList (Left e) = [e]
    eitherUnitToErrorList (Right ()) = []

validateUserEntity :: AppSpec -> [ValidationError]
validateUserEntity spec =
  case App.auth (snd $ getApp spec) of
    Nothing -> []
    Just auth ->
      case Entity.getIdField userEntity of
        Nothing -> [userEntityMissingIdFieldError]
        Just idField ->
          if Psl.Util.doesPslFieldHaveAttribute "default" idField
            then []
            else [userEntityIdFieldMissingDefaultAttrError]
      where
        (userEntityName, userEntity) = AS.resolveRef spec (Auth.userEntity auth)

        userEntityMissingIdFieldError = GenericValidationError $ "Entity '" ++ userEntityName ++ "' (referenced by app.auth.userEntity) must have an ID field (specified with the '@id' attribute)"
        userEntityIdFieldMissingDefaultAttrError = GenericValidationError $ "Entity '" ++ userEntityName ++ "' (referenced by app.auth.userEntity) must have an ID field (specified with the '@id' attribute) with a default value"

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
      ("The database provider in the schema.prisma file must be \"" ++ Psl.Db.dbProviderPostgresqlStringLiteral ++ "\" since there are jobs with executor set to PgBoss.")
    | isPgBossJobExecutorUsed spec && not (isPostgresUsed spec)
  ]

validateEmailSenderIsDefinedIfEmailAuthIsUsed :: AppSpec -> [ValidationError]
validateEmailSenderIsDefinedIfEmailAuthIsUsed spec = case App.auth app of
  Nothing -> []
  Just auth ->
    if Auth.isEmailAuthEnabled auth && isNothing (App.emailSender app)
      then [GenericValidationError "app.emailSender must be specified when using email auth. You can use the Dummy email sender for development purposes."]
      else []
  where
    app = snd $ getApp spec

validateDummyEmailSenderIsNotUsedInProduction :: AppSpec -> [ValidationError]
validateDummyEmailSenderIsNotUsedInProduction spec =
  if AS.isBuild spec && isDummyEmailSenderUsed
    then [GenericValidationError "app.emailSender must not be set to Dummy when building for production."]
    else []
  where
    isDummyEmailSenderUsed = (AS.EmailSender.provider <$> App.emailSender app) == Just AS.EmailSender.Dummy
    app = snd $ getApp spec

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

{- ORMOLU_DISABLE -}
-- *** MAKE SURE TO UPDATE: Unit tests in `AppSpec.ValidTest` module named "duplicate declarations validation"
-- to include the new declaration type.
{- ORMOLU_ENABLE -}
validateUniqueDeclarationNames :: AppSpec -> [ValidationError]
validateUniqueDeclarationNames spec =
  concat
    [ checkIfDeclarationsAreUnique "page" (AS.getPages spec),
      checkIfDeclarationsAreUnique "route" (AS.getRoutes spec),
      checkIfDeclarationsAreUnique "action" (AS.getActions spec),
      checkIfDeclarationsAreUnique "query" (AS.getQueries spec),
      checkIfDeclarationsAreUnique "api" (AS.getApis spec),
      checkIfDeclarationsAreUnique "apiNamespace" (AS.getApiNamespaces spec),
      checkIfDeclarationsAreUnique "crud" (AS.getCruds spec),
      checkIfDeclarationsAreUnique "entity" (AS.getEntities spec),
      checkIfDeclarationsAreUnique "job" (AS.getJobs spec)
    ]
  where
    checkIfDeclarationsAreUnique :: IsDecl a => String -> [(String, a)] -> [ValidationError]
    checkIfDeclarationsAreUnique declTypeName decls = case duplicateDeclNames of
      [] -> []
      (firstDuplicateDeclName : _) ->
        [ GenericValidationError $
            "There are duplicate "
              ++ declTypeName
              ++ " declarations with name '"
              ++ firstDuplicateDeclName
              ++ "'."
        ]
      where
        duplicateDeclNames :: [String]
        duplicateDeclNames = findDuplicateElems $ map fst decls

validateDeclarationNames :: AppSpec -> [ValidationError]
validateDeclarationNames spec =
  concat
    [ capitalizedOperationsErrorMessage,
      capitalizedJobsErrorMessage,
      nonCapitalizedEntitesErrorMessage
    ]
  where
    capitalizedOperationsErrorMessage =
      let capitalizedOperationNames = filter isCapitalized $ map AS.Operation.getName $ AS.getOperations spec
       in case capitalizedOperationNames of
            [] -> []
            _ ->
              [ GenericValidationError $
                  "Operation names must start with a lowercase letter. Please rename operations: "
                    ++ intercalate ", " capitalizedOperationNames
                    ++ "."
              ]

    capitalizedJobsErrorMessage =
      let capitalizedJobNames = filter isCapitalized $ map fst $ AS.getJobs spec
       in case capitalizedJobNames of
            [] -> []
            _ ->
              [ GenericValidationError $
                  "Job names must start with a lowercase letter. Please rename jobs: "
                    ++ intercalate ", " capitalizedJobNames
                    ++ "."
              ]

    nonCapitalizedEntitesErrorMessage =
      let nonCapitalizedEntitieNames = filter (not . isCapitalized) $ map fst $ AS.getEntities spec
       in case nonCapitalizedEntitieNames of
            [] -> []
            _ ->
              [ GenericValidationError $
                  "Entity names must start with an uppercase letter. Please rename entities: "
                    ++ intercalate ", " nonCapitalizedEntitieNames
                    ++ "."
              ]

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

validateUserNodeVersionRange :: AppSpec -> [ValidationError]
validateUserNodeVersionRange spec =
  concat
    [ checkUserRangeIsInWaspRange,
      checkUserRangeDoesNotAllowMajorChanges
    ]
  where
    userRange = AS.userNodeVersionRange spec

    checkUserRangeIsInWaspRange :: [ValidationError]
    checkUserRangeIsInWaspRange =
      if not (V.isRangeInWaspSupportedRange userRange)
        then
          [ GenericValidationError $
              "Your app's Node version range ("
                <> show userRange
                <> ") allows versions lower than "
                <> show oldestWaspSupportedNodeVersion
                <> "."
                <> " Wasp only works with Node >= "
                <> show oldestWaspSupportedNodeVersion
                <> "."
          ]
        else []

    checkUserRangeDoesNotAllowMajorChanges :: [ValidationError]
    checkUserRangeDoesNotAllowMajorChanges =
      if SV.doesVersionRangeAllowMajorChanges userRange
        then
          [ GenericValidationWarning $
              "Your app's Node version range ("
                <> show userRange
                <> ") allows breaking changes."
                <> "To ensure consistency between development and production environments,"
                <> " we recommend you narrow down your Node version range to not allow breaking changes."
          ]
        else []

validateAtLeastOneRoute :: AppSpec -> [ValidationError]
validateAtLeastOneRoute spec =
  if null routes
    then
      [ GenericValidationError
          "You must have at least one route in your app. You can add it using the 'route' declaration."
      ]
    else []
  where
    routes = AS.getRoutes spec

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

getValidDbSystem :: AppSpec -> AS.Db.DbSystem
getValidDbSystem = getValidDbSystemFromPrismaSchema . AS.prismaSchema

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
isPostgresUsed :: AppSpec -> Bool
isPostgresUsed = (AS.Db.PostgreSQL ==) . getValidDbSystem

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
getIdFieldFromCrudEntity :: AppSpec -> AS.Crud.Crud -> Psl.Model.Field
getIdFieldFromCrudEntity spec crud = fromJust $ Entity.getIdField crudEntity
  where
    crudEntity = snd $ AS.resolveRef spec (AS.Crud.entity crud)

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
-- Example: If user specified their node version range to be [22.12, 24), then this function will return 22.12.
getLowestNodeVersionUserAllows :: AppSpec -> SV.Version
getLowestNodeVersionUserAllows spec =
  fromMaybe (error "This should never happen: user Node version range lower bound is Inf") $
    SVB.versionFromBound $ fst $ SVB.versionBounds $ AS.userNodeVersionRange spec
