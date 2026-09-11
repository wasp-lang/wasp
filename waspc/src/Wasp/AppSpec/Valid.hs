{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Valid
  ( validateAppSpec,
    getApp,
    isAuthEnabled,
    getDeploymentMode,
    doesUserEntityContainField,
    getIdFieldFromCrudEntity,
    getLowestNodeVersionUserAllows,
    getValidDbSystem,
    isSingleDeploymentAndDevelopment,
    areWebSocketsUsed,
    ServerPath (..),
    ClaimedHttpMethods (..),
    getServerPaths,
    serverPathClaimedPath,
    serverPathClaimedHttpMethods,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.List (find, groupBy, intercalate, sortBy, sortOn)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Parsec as P
import Wasp.Analyzer.AST (isValidWaspIdentifier)
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
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.AppSpec.App.Wasp as Wasp
import Wasp.AppSpec.Core.Decl (getDeclName, takeDecls)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Route as Route
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.Node.Version (oldestWaspSupportedNodeVersion)
import qualified Wasp.Node.Version as V
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Db as Psl.Db
import qualified Wasp.Psl.Util as Psl.Util
import Wasp.Psl.Valid (getValidDbSystemFromPrismaSchema)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.SemanticVersion.VersionBound as SVB
import qualified Wasp.ServerRoutes as ServerRoutes
import Wasp.Util (findDuplicateElems, indent, isCapitalized)
import Wasp.Util.InstallMethod (getInstallationCommand)
import Wasp.Util.UrlPath (getStaticPathPrefix, isPathSegmentPrefixOf, stripTrailingSlashes)
import Wasp.Util.WebRouterPath (doesConcretePathMatchRoutePattern)
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
          validateOperationEntitiesAreUnique spec,
          validateUniqueDeclarationNames spec,
          validateDeclarationNames spec,
          validateWebAppBaseDir spec,
          validateUserApisDoNotCollideWithWaspRoutes spec,
          validatePageRoutesAreNotUnderServerPathPrefixes spec,
          validateUserNodeVersionRange spec,
          validateAtLeastOneRoute spec,
          validatePrerenderRoutes spec
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
  specWaspVersionRange <- first parseErrorToValidationError $ SV.parseRange specWaspVersionStr
  unless (SV.isVersionInRange WV.waspVersion specWaspVersionRange) $
    Left (incompatibleVersionError WV.waspVersion specWaspVersionRange)
  where
    -- Currently the 'ParseError' does not give user-friendly information,
    -- so we discard it for a generic error.
    parseErrorToValidationError :: P.ParseError -> ValidationError
    parseErrorToValidationError _err =
      GenericValidationError $
        unlines
          [ "Invalid Wasp version requirement: " ++ specWaspVersionStr,
            "Make sure to use a npm-compatible version range.",
            "For example: "
              ++ show (SV.backwardsCompatibleWith WV.waspVersion)
              ++ ", "
              ++ show (SV.approximatelyEquivalentTo WV.waspVersion)
              ++ " or "
              ++ show (SV.eq WV.waspVersion)
          ]

    incompatibleVersionError :: SV.Version -> SV.Range -> ValidationError
    incompatibleVersionError actualVersion expectedVersionRange =
      GenericValidationError $
        unlines
          [ "Your Wasp version does not match the app's requirements.",
            "You are running Wasp " ++ show actualVersion ++ ".",
            "This app requires Wasp " ++ show expectedVersionRange ++ ".",
            "To install a specific version of Wasp, do:",
            indent 2 $ getInstallationCommand $ Just "x.y.z",
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
    anyPageRequiresAuth = any ((== Just True) . Page.authRequired . snd) (AS.getPages spec)

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
  if AS.isProduction spec && isDummyEmailSenderUsed
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
    duplicatePaths = findDuplicateElems namespacePaths

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
        crudOperations = AS.Crud.toOperationList crud.operations

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

validateOperationEntitiesAreUnique :: AppSpec -> [ValidationError]
validateOperationEntitiesAreUnique spec =
  concatMap validateOperation (AS.getOperations spec)
  where
    validateOperation :: AS.Operation.Operation -> [ValidationError]
    validateOperation operation = case findDuplicateElems entityNames of
      [] -> []
      duplicateEntityNames ->
        [ GenericValidationError $
            "The "
              ++ describeOperation operation
              ++ " lists the same entity more than once in its 'entities' list: "
              ++ intercalate ", " (map show duplicateEntityNames)
              ++ ". Please remove the duplicate entity references."
        ]
      where
        entityNames = maybe [] (map AS.refName) (AS.Operation.getEntities operation)

    describeOperation :: AS.Operation.Operation -> String
    describeOperation (AS.Operation.QueryOp name _) = "query '" ++ name ++ "'"
    describeOperation (AS.Operation.ActionOp name _) = "action '" ++ name ++ "'"

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
    checkIfDeclarationsAreUnique :: (IsDecl a) => String -> [(String, a)] -> [ValidationError]
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
    [ declNameIsNotAValidIdentifierErrorMessage,
      capitalizedOperationsErrorMessage,
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

    declNameIsNotAValidIdentifierErrorMessage =
      {-
        NOTE: This check is only relevant if the user is using the TS spec. If
        the user is using the DSL, the check is redundant and will never
        trigger.

        More precisely:
        - DSL - If a declaration name isn't a valid identifier, the lexer
          doesn't tokenize it and stops the compilation much earlier with a
          syntax error.
        - TS Spec - Since declaration names come from TypeScript
          strings, they can still be anything by this point. The check here
          ensures that declarations in the TS spec follow the same rules as
          the DSL.

        It would be more consistent to perform this check much earlier,
        probably in TypeScript. We decided to put it here because:
        - This is where we keep similar app spec validations.
        - It reuses the actual lexer instead of duplicating its rules in
          TypeScript (and in potential future spec runtimes).
      -}
      let invalidIdentifierDeclNames = filter (not . isValidWaspIdentifier) $ map getDeclName $ AS.decls spec
          waspIdentifierNameRules =
            [ "must start with a letter or an underscore",
              "must contain only letters, numbers, or underscores",
              "must not be a Wasp keyword"
            ]
       in case invalidIdentifierDeclNames of
            [] -> []
            _ ->
              [ GenericValidationError $
                  intercalate "\n" $
                    ("Please rename: " ++ intercalate ", " invalidIdentifierDeclNames ++ ". Each declaration name:")
                      : map (indent 2 . ("- " ++)) waspIdentifierNameRules
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

-- | Wasp registers its own routes ahead of the user's apis, so an api that shares both the
-- path and the method with one of them gets shadowed. We throw warnings in such cases.
validateUserApisDoNotCollideWithWaspRoutes :: AppSpec -> [ValidationError]
validateUserApisDoNotCollideWithWaspRoutes spec =
  concatMap validateUserApi (AS.getApis spec)
  where
    validateUserApi (apiName, api)
      | apiPath == healthRoutePath && apiAnswersTo AS.Api.GET =
          [ GenericValidationWarning $
              "The api '"
                ++ apiName
                ++ "' has path "
                ++ show (AS.Api.path api)
                ++ ", which is Wasp's own health check route, so Wasp's route would answer instead of the api."
          ]
      | apiPath `elem` waspOperationAndCrudRoutePaths && apiAnswersTo AS.Api.POST =
          [ GenericValidationWarning $
              "The api '"
                ++ apiName
                ++ "' has path "
                ++ show (AS.Api.path api)
                ++ ", which is one of Wasp's own routes, so Wasp's route would answer instead of the api."
          ]
      | otherwise = []
      where
        apiPath = stripTrailingSlashes $ AS.Api.path api

        -- Operations and cruds are all POST, the health check is GET.
        apiAnswersTo httpMethod = AS.Api.method api `elem` [httpMethod, AS.Api.ALL]

    waspOperationAndCrudRoutePaths = getWaspOperationAndCrudRoutePaths spec

-- TODO: Add `server.basePath` so users can move Wasp's routes and their apis under a
-- prefix of their own (e.g. `/api`), which is the real fix for these collisions instead
-- of asking them to rename their pages. Once it exists, point this warning at it as the
-- way to resolve the conflict.

-- | In single deployment mode, the client serves pages requested with `GET`, so a page route
-- sharing a path with a server `GET` route gets shadowed. We throw warnings in such cases.
-- Sharing a path with e.g. a `POST`-only api is fine, the browser's `GET` still reaches the client.
validatePageRoutesAreNotUnderServerPathPrefixes :: AppSpec -> [ValidationError]
validatePageRoutesAreNotUnderServerPathPrefixes spec
  | getDeploymentMode spec == Split = []
  | otherwise =
      [ GenericValidationWarning $
          "The route '"
            ++ routeName
            ++ "' has path "
            ++ show (Route.path route)
            ++ ", which is under "
            ++ show (serverPathClaimedPath serverPath)
            ++ ", a path handled by the server (Wasp's own routes and your apis), so the server would answer instead of the page."
      | (routeName, route) <- AS.getRoutes spec,
        serverPath <- getServerPaths spec,
        serverPathClaimsHttpMethod serverPath AS.Api.GET (Route.path route)
      ]

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

validatePrerenderRoutes :: AppSpec -> [ValidationError]
validatePrerenderRoutes spec =
  concatMap validatePrerenderRoute prerenderRoutes
  where
    -- Routes that prerender at least one path.
    prerenderRoutes = filter (not . null . prerenderPaths . snd) (AS.getRoutes spec)

    validatePrerenderRoute (routeName, route) =
      concatMap (validatePrerenderPath routeName route) (prerenderPaths route)
        ++ [ GenericValidationError $
               "Route '"
                 ++ routeName
                 ++ "' has prerendering enabled but its page has authRequired set to true."
                 ++ " Prerendered routes cannot require authentication."
           | pageRequiresAuth (getPage route)
           ]

    validatePrerenderPath routeName route path
      | pathHasDynamicSegments path =
          [ GenericValidationError $
              "Route '"
                ++ routeName
                ++ "' lists prerender path ("
                ++ path
                ++ ") which contains dynamic segments. Prerender paths must be fully static."
          ]
      | not (doesConcretePathMatchRoutePattern (Route.path route) path) =
          [ GenericValidationError $
              "Route '"
                ++ routeName
                ++ "' lists prerender path ("
                ++ path
                ++ ") which does not match the route's path pattern ("
                ++ Route.path route
                ++ ")."
          ]
      | otherwise = []

    prerenderPaths = Route.prerender
    pathHasDynamicSegments path = any (`elem` path) [':', '*', '?']
    pageRequiresAuth page = Page.authRequired page == Just True

    getPage route = snd $ AS.resolveRef spec (Route.to route)

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
getDeploymentMode :: AppSpec -> DeploymentMode
getDeploymentMode = App.getDeploymentMode . snd . getApp

-- | Single deployment mode in a development build, where the client dev server proxies the server's routes.
-- This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
isSingleDeploymentAndDevelopment :: AppSpec -> Bool
isSingleDeploymentAndDevelopment spec = getDeploymentMode spec == Single && AS.isDevelopment spec

-- | This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
-- TODO: This is here only because defining it in `Wasp.Generator.WebSocket` would give
-- us cyclic imports. The whole `Valid` module needs a cleanup.
areWebSocketsUsed :: AppSpec -> Bool
areWebSocketsUsed spec = isJust (App.webSocket $ snd $ getApp spec)

-- | A path the server answers on, and the methods it answers with. We claim a subtree only
-- when we cannot name the path in full, so the client keeps the pages the server does not
-- serve. An unclaimed `GET` renders a page; every other method stays with the server.
data ServerPath
  = -- | Exactly this path, and nothing under it.
    ExactPath String ClaimedHttpMethods
  | -- | This path and everything under it.
    SubtreePath String ClaimedHttpMethods
  deriving (Show, Eq)

-- | The methods a server path answers on.
data ClaimedHttpMethods
  = -- | Every httpMethod there is, for paths whose routes we cannot enumerate (and so neither
    -- can we their methods), and for an `api` declared as `ALL`.
    AllHttpMethods
  | OnlyHttpMethods (Set AS.Api.HttpMethod)
  deriving (Show, Eq)

-- | The path a server path claims. Used in the warning message.
serverPathClaimedPath :: ServerPath -> String
serverPathClaimedPath (ExactPath serverPath _) = serverPath
serverPathClaimedPath (SubtreePath serverPath _) = serverPath

-- | The methods a server path claims.
serverPathClaimedHttpMethods :: ServerPath -> ClaimedHttpMethods
serverPathClaimedHttpMethods (ExactPath _ httpMethods) = httpMethods
serverPathClaimedHttpMethods (SubtreePath _ httpMethods) = httpMethods

-- | Whether the server path takes over the given path, whatever the method.
serverPathClaims :: ServerPath -> String -> Bool
serverPathClaims (ExactPath serverPath _) path = stripTrailingSlashes serverPath == stripTrailingSlashes path
serverPathClaims (SubtreePath serverPath _) path = serverPath `isPathSegmentPrefixOf` path

-- | Whether the server path takes over the given path for the given method. An unclaimed
-- `GET` is the client's, so a `GET` on a `POST`-only path renders the page rather than 404ing.
serverPathClaimsHttpMethod :: ServerPath -> AS.Api.HttpMethod -> String -> Bool
serverPathClaimsHttpMethod serverPath httpMethod path =
  serverPath `serverPathClaims` path && claimsMethod (serverPathClaimedHttpMethods serverPath)
  where
    claimsMethod AllHttpMethods = True
    claimsMethod (OnlyHttpMethods httpMethods) = httpMethod `Set.member` httpMethods

-- | Every path the server answers on: Wasp's own routes and the user's apis.
-- This function assumes that @AppSpec@ it operates on was validated beforehand (with @validateAppSpec@ function).
getServerPaths :: AppSpec -> [ServerPath]
getServerPaths spec = mergeServerPathsClaimingSamePath $ waspServerPaths ++ userApiServerPaths
  where
    waspServerPaths =
      map (`ExactPath` onlyHttpMethod AS.Api.POST) (getWaspOperationAndCrudRoutePaths spec)
        ++ [ExactPath healthRoutePath (onlyHttpMethod AS.Api.GET)]
        -- We cannot enumerate the routes under these paths, so neither can we their methods.
        ++ map (`SubtreePath` AllHttpMethods) waspSubtreePaths

    -- TODO: Auth claims its whole subtree because there is no easy way to derive all of its
    -- routes: they are spread across the auth generator and the enabled providers. Enumerate
    -- them so that auth claims exact paths too.
    waspSubtreePaths =
      map ("/" ++) $
        [ServerRoutes.authRouteInRootRouter | isAuthEnabled spec]
          -- Socket.IO owns this whole path space, including its polling endpoints.
          ++ [ServerRoutes.webSocketRouteInRootRouter | areWebSocketsUsed spec]

    -- An `apiNamespace` claims nothing: it only customises middleware for the apis under
    -- it, and each of those claims its own path. Middleware that answers requests itself,
    -- such as one serving Swagger UI, is the user's own mount, like a route added in
    -- `setupFn`, so they proxy it with their own `server.proxy` entry.
    userApiServerPaths = map (toServerPath . AS.Api.httpRoute . snd) (AS.getApis spec)

    -- A path we can spell out in full is claimed exactly. One with a pattern segment has no
    -- such spelling, so we claim the subtree under its static prefix. For `/:id` that
    -- subtree is the whole site.
    toServerPath (httpMethod, path)
      | staticPathPrefix == stripTrailingSlashes path = ExactPath staticPathPrefix httpMethods
      | otherwise = SubtreePath staticPathPrefix httpMethods
      where
        staticPathPrefix = stripTrailingSlashes $ getStaticPathPrefix path
        httpMethods = case httpMethod of
          AS.Api.ALL -> AllHttpMethods
          _ -> onlyHttpMethod httpMethod

    onlyHttpMethod = OnlyHttpMethods . Set.singleton

-- | Several apis, or an api and one of Wasp's routes, can claim the same path with different
-- methods. They have to end up as one server path claiming all of those methods, so that
-- whoever reads these (e.g. the dev proxy, which keys its entries by path) sees them all.
mergeServerPathsClaimingSamePath :: [ServerPath] -> [ServerPath]
mergeServerPathsClaimingSamePath = map mergeGroup . groupBy claimTheSame . sortOn claimKey
  where
    claimKey serverPath = (isSubtree serverPath, serverPathClaimedPath serverPath)
    claimTheSame a b = claimKey a == claimKey b

    isSubtree (SubtreePath _ _) = True
    isSubtree (ExactPath _ _) = False

    mergeGroup serverPaths@(firstServerPath : _) =
      withClaimedHttpMethods firstServerPath $ foldr1 unionHttpMethods (map serverPathClaimedHttpMethods serverPaths)
    mergeGroup [] = error "Impossible: groupBy never produces an empty group."

    withClaimedHttpMethods (ExactPath path _) httpMethods = ExactPath path httpMethods
    withClaimedHttpMethods (SubtreePath path _) httpMethods = SubtreePath path httpMethods

    unionHttpMethods AllHttpMethods _ = AllHttpMethods
    unionHttpMethods _ AllHttpMethods = AllHttpMethods
    unionHttpMethods (OnlyHttpMethods a) (OnlyHttpMethods b) = OnlyHttpMethods (a `Set.union` b)

-- | The path of the health check route the server registers.
healthRoutePath :: String
healthRoutePath = "/" ++ ServerRoutes.healthRouteInRootRouter

-- | The full path of every operation and CRUD route the server registers.
getWaspOperationAndCrudRoutePaths :: AppSpec -> [String]
getWaspOperationAndCrudRoutePaths spec =
  [ "/" ++ ServerRoutes.operationsRouteInRootRouter ++ "/" ++ ServerRoutes.operationRouteInOperationsRouter operation
  | operation <- AS.getOperations spec
  ]
    ++ [ "/" ++ ServerRoutes.makeCrudOperationFullPath crudName crudOperation
       | (crudName, crud) <- AS.getCruds spec,
         (crudOperation, _) <- AS.Crud.toOperationList (AS.Crud.operations crud)
       ]

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

findFieldByName :: String -> [Psl.Model.Field] -> Maybe Psl.Model.Field
findFieldByName name = find ((== name) . Psl.Model._name)

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
    SVB.versionFromBound $
      fst $
        SVB.versionBounds $
          AS.userNodeVersionRange spec
