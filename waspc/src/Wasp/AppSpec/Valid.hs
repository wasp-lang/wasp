{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Valid
  ( validateAppSpec,
    getApp,
    isAuthEnabled,
    isWaspAuthUsed,
    getExternalAuthProviders,
    doesUserEntityContainField,
    getIdFieldFromCrudEntity,
    getLowestNodeVersionUserAllows,
    getValidDbSystem,
  )
where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.List (find, group, groupBy, intercalate, isPrefixOf, sort, sortBy, tails)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Text.Parsec as P
import Wasp.Analyzer.AST (isValidWaspIdentifier)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
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
import qualified Wasp.AppSpec.AuthRequirement as AuthRequirement
import Wasp.AppSpec.Core.Decl (getDeclName, takeDecls)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Query as AS.Query
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
import Wasp.Util (findDuplicateElems, indent, isCapitalized)
import Wasp.Util.InstallMethod (getInstallationCommand)
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
          validateAuthProviders spec,
          validateAuthRequirements spec,
          validateDummyEmailSenderIsNotUsedInProduction spec,
          validateDbIsPostgresIfPgBossUsed spec,
          validateApiRoutesAreUnique spec,
          validateApiNamespacePathsAreUnique spec,
          validateCrudOperations spec,
          validateOperationEntitiesAreUnique spec,
          validateUniqueDeclarationNames spec,
          validateDeclarationNames spec,
          validateWebAppBaseDir spec,
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
    anyPageRequiresAuth = any (AuthRequirement.isAuthRequiredWithDefault False . Page.authRequired) (snd <$> AS.getPages spec)

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

-- | Coherence checks for an external auth provider manifest.
--
-- Wasp-auth config next to an external provider needs no check here: each
-- provider is a sum type, so that state is unrepresentable. What remains are
-- data-level properties the types cannot express: per-provider route checks,
-- and the cross-provider properties (unique ids, non-colliding route mounts,
-- non-colliding env vars) that only exist now that providers are a list.
validateAuthProviders :: AppSpec -> [ValidationError]
validateAuthProviders spec = case App.auth (snd $ getApp spec) of
  Nothing -> []
  Just auth ->
    concat
      [ [ GenericValidationError "app.auth.providers must contain at least one provider."
        | null (Auth.providers auth)
        ],
        validateProviderIdsAreUnique auth,
        concatMap validateExternalProviderIdPrefix (Auth.externalProviders auth),
        concatMap validateCookieTransportImpliesRevocation (Auth.externalProviders auth),
        concatMap validateRoutesBasePath (Auth.externalProviders auth),
        validateProviderBasePathsDoNotOverlap (Auth.externalProviders auth),
        validateProviderEnvVarsDoNotCollide (Auth.externalProviders auth),
        concatMap validateProviderEnvVarsAreNotReserved (Auth.externalProviders auth),
        concatMap validateProviderUses (Auth.externalProviders auth),
        concatMap validateProviderIdentityNamespaces (Auth.externalProviders auth),
        validateIdentityNamespacesAreDisjoint (Auth.externalProviders auth),
        concatMap (validateEmailSendGrantHasEmailSender spec) (Auth.externalProviders auth)
      ]
  where
    validateProviderIdsAreUnique auth =
      map duplicateIdError $ findDuplicateElems (Auth.authProviderId <$> Auth.providers auth)
      where
        duplicateIdError duplicateId
          | duplicateId == Auth.waspAuthProviderId =
              GenericValidationError "app.auth.providers may contain at most one waspAuth(...) provider."
          | otherwise =
              GenericValidationError $
                "app.auth.providers contains provider id '"
                  ++ duplicateId
                  ++ "' more than once. Identities are recorded under this id, so each provider may appear"
                  ++ " at most once (provider instance ids are not configurable yet)."

    -- The TS mapper enforces the same rule; this mirror covers every entry
    -- point that does not go through the TS spec (and any future one).
    validateExternalProviderIdPrefix extProvider =
      [ GenericValidationError $
          "Auth provider id '"
            ++ extProvider.providerId
            ++ "' must start with 'external:' (e.g. 'external:clerk'). The unprefixed namespace is"
            ++ " reserved for Wasp's own auth methods, which record identities in the same place --"
            ++ " the prefix is what makes a collision impossible."
      | not ("external:" `isPrefixOf` extProvider.providerId)
      ]

    -- A cookie-borne credential Wasp cannot revoke server-side would make
    -- logout() a lie: the next visitor of a shared computer silently
    -- re-authenticates.
    validateCookieTransportImpliesRevocation extProvider =
      [ GenericValidationError $
          "Auth provider '"
            ++ extProvider.providerId
            ++ "' declares the 'cookie-transport' capability without 'session-revocation'. A provider"
            ++ " whose credential lives in a cookie must be able to revoke sessions server-side, or"
            ++ " logout would only appear to work."
      | "cookie-transport" `elem` extProvider.capabilities,
        "session-revocation" `notElem` extProvider.capabilities
      ]

    -- Adapter runtimes receive exactly the env vars their manifest declared,
    -- so a manifest declaring a framework-owned name (JWT_SECRET) would be
    -- handed the framework's secret through the sanctioned channel. Mirrors
    -- reservedServerEnvVarNames / reservedClientEnvVarNames in the TS spec
    -- package (spec/src/spec/authReservedEnvVarNames.ts) and the names owned
    -- by the generated server env schema (sdk/wasp/server/env.ts template).
    validateProviderEnvVarsAreNotReserved extProvider =
      reservedNameErrors "server" extProvider.envVars.server reservedServerEnvVarNames
        ++ reservedNameErrors "client" extProvider.envVars.client reservedClientEnvVarNames
      where
        reservedNameErrors side envVars reservedNames =
          [ GenericValidationError $
              "Auth provider '"
                ++ extProvider.providerId
                ++ "' declares the "
                ++ side
                ++ " env var '"
                ++ envVar.name
                ++ "', which Wasp owns. Framework env var names cannot be declared by providers;"
                ++ " pick a provider-specific name."
          | envVar <- envVars,
            envVar.name `elem` reservedNames
          ]
        reservedServerEnvVarNames =
          [ "NODE_ENV",
            "PORT",
            "DATABASE_URL",
            "PG_BOSS_NEW_OPTIONS",
            "WASP_SERVER_URL",
            "WASP_WEB_CLIENT_URL",
            "JWT_SECRET",
            "SKIP_EMAIL_VERIFICATION_IN_DEV",
            "SMTP_HOST",
            "SMTP_PORT",
            "SMTP_USERNAME",
            "SMTP_PASSWORD",
            "SENDGRID_API_KEY",
            "MAILGUN_API_KEY",
            "MAILGUN_DOMAIN",
            "MAILGUN_API_URL",
            "RESEND_API_KEY",
            "GOOGLE_CLIENT_ID",
            "GOOGLE_CLIENT_SECRET",
            "GITHUB_CLIENT_ID",
            "GITHUB_CLIENT_SECRET",
            "SLACK_CLIENT_ID",
            "SLACK_CLIENT_SECRET",
            "DISCORD_CLIENT_ID",
            "DISCORD_CLIENT_SECRET",
            "KEYCLOAK_CLIENT_ID",
            "KEYCLOAK_CLIENT_SECRET",
            "KEYCLOAK_REALM_URL",
            "MICROSOFT_TENANT_ID",
            "MICROSOFT_CLIENT_ID",
            "MICROSOFT_CLIENT_SECRET"
          ]
        reservedClientEnvVarNames = ["NODE_ENV", "REACT_APP_API_URL"]

    -- Grants are a closed set: the generator can only wire facets it knows,
    -- so an unknown name must be an error, not an absent property at runtime.
    validateProviderUses extProvider =
      [ GenericValidationError $
          "Auth provider '"
            ++ extProvider.providerId
            ++ "' requests the unknown runtime grant '"
            ++ grantName
            ++ "'. Known grants: "
            ++ intercalate ", " knownRuntimeGrantNames
            ++ "."
      | grantName <- extProvider.uses,
        grantName `notElem` knownRuntimeGrantNames
      ]
      where
        knownRuntimeGrantNames = ["wasp-sessions", "email-send", "identity-namespaces"]

    -- A provider owns its manifest id and anything under `id ++ "/"`; that
    -- shape is what makes cross-provider identity collisions impossible by
    -- construction. Using more than the default namespace requires the
    -- 'identity-namespaces' grant, so the power shows up in `uses`.
    validateProviderIdentityNamespaces extProvider =
      concat
        [ [ GenericValidationError $
              "Auth provider '"
                ++ extProvider.providerId
                ++ "' declares the identity namespace '"
                ++ namespace
                ++ "', which it does not own. A namespace must be the provider id or '"
                ++ extProvider.providerId
                ++ "/<suffix>' -- that rule is what makes cross-provider identity collisions impossible."
          | namespace <- extProvider.identityNamespaces,
            not (isOwnNamespace namespace)
          ],
          [ GenericValidationError $
              "Auth provider '" ++ extProvider.providerId ++ "' declares a duplicate identity namespace."
          | not (null (findDuplicateElems extProvider.identityNamespaces))
          ],
          [ GenericValidationError $
              "Auth provider '"
                ++ extProvider.providerId
                ++ "' declares identity namespaces beyond its default one, which requires the"
                ++ " 'identity-namespaces' grant in `uses`."
          | usesNamespacesBeyondDefault,
            "identity-namespaces" `notElem` extProvider.uses
          ]
        ]
      where
        isOwnNamespace namespace =
          namespace == extProvider.providerId
            || ( (extProvider.providerId ++ "/") `isPrefixOf` namespace
                   && length namespace > length extProvider.providerId + 1
               )
        usesNamespacesBeyondDefault =
          extProvider.identityNamespaces /= [extProvider.providerId]

    -- Belt and braces on top of the per-provider ownership rule: even if the
    -- shape rule ever loosens, two providers may never share a namespace,
    -- because identities are recorded under it.
    validateIdentityNamespacesAreDisjoint extProviders =
      [ GenericValidationError $
          "Auth providers "
            ++ intercalate " and " (map (\ownerId -> "'" ++ ownerId ++ "'") ownerIds)
            ++ " both declare the identity namespace '"
            ++ namespace
            ++ "'. Identities are recorded under the namespace, so each one must belong to exactly one provider."
      | (namespace, ownerIds) <- duplicatedNamespacesWithOwners
      ]
      where
        namespaceOwnership =
          [ (namespace, extProvider.providerId)
          | extProvider <- extProviders,
            namespace <- extProvider.identityNamespaces
          ]
        duplicatedNamespacesWithOwners =
          [ (namespace, snd <$> ownerships)
          | ownerships@((namespace, _) : _ : _) <-
              groupBy (\a b -> fst a == fst b) $ sortBy (\a b -> compare (fst a) (fst b)) namespaceOwnership
          ]

    -- Generalizes validateEmailSenderIsDefinedIfEmailAuthIsUsed to adapters:
    -- an email-sending provider cannot ship into an app that would silently
    -- drop its emails.
    validateEmailSendGrantHasEmailSender spec' extProvider =
      [ GenericValidationError $
          "Auth provider '"
            ++ extProvider.providerId
            ++ "' requests the 'email-send' grant, which requires app.emailSender to be specified."
      | "email-send" `elem` extProvider.uses,
        isNothing (App.emailSender (snd $ getApp spec'))
      ]

    validateRoutesBasePath extProvider = case Auth.routes extProvider of
      Nothing -> []
      Just providerRoutes ->
        let bPath = providerRoutes.basePath
            reservedPathPrefixes = ["/auth", "/operations", "/crud"]
            declaredApiPaths =
              (AS.ApiNamespace.path . snd <$> AS.getApiNamespaces spec)
                ++ (snd . AS.Api.httpRoute . snd <$> AS.getApis spec)
         in concat
              [ [ GenericValidationError $
                    "Auth provider '" ++ extProvider.providerId ++ "' routes basePath must start with '/', got: " ++ bPath
                | not ("/" `isPrefixOf` bPath)
                ],
                [ GenericValidationError $
                    "Auth provider '" ++ extProvider.providerId ++ "' routes basePath '" ++ bPath ++ "' collides with a path Wasp reserves (" ++ intercalate ", " reservedPathPrefixes ++ ")."
                | any (`isPrefixOf` bPath) reservedPathPrefixes
                ],
                [ GenericValidationError $
                    "Auth provider '" ++ extProvider.providerId ++ "' routes basePath '" ++ bPath ++ "' collides with a declared api or apiNamespace path."
                | any (\apiPath -> bPath `isPrefixOf` apiPath || apiPath `isPrefixOf` bPath) declaredApiPaths
                ]
              ]

    validateProviderBasePathsDoNotOverlap extProviders =
      [ GenericValidationError $
          "Auth providers '"
            ++ providerIdA
            ++ "' and '"
            ++ providerIdB
            ++ "' have overlapping routes basePaths ('"
            ++ basePathA
            ++ "' and '"
            ++ basePathB
            ++ "'). Each provider's routes must mount under a distinct path."
      | ((providerIdA, basePathA) : rest) <- tails providerBasePaths,
        (providerIdB, basePathB) <- rest,
        isPathPrefixOfPath basePathA basePathB || isPathPrefixOfPath basePathB basePathA
      ]
      where
        providerBasePaths =
          [ (extProvider.providerId, providerRoutes.basePath)
          | extProvider <- extProviders,
            Just providerRoutes <- [Auth.routes extProvider]
          ]
        -- Prefix on segment boundaries: /better-auth prefixes /better-auth/x
        -- but not /better-auth-2.
        isPathPrefixOfPath pathA pathB = splitPathSegments pathA `isPrefixOf` splitPathSegments pathB
        splitPathSegments = filter (not . null) . foldr splitOnSlash [[]]
          where
            splitOnSlash '/' segments = [] : segments
            splitOnSlash c (segment : segments) = (c : segment) : segments
            splitOnSlash c [] = [[c]]

    -- Two providers declaring the same env var name is always an error: even
    -- an identically named and typed variable is separate per-instance
    -- configuration, and process.env has one global namespace.
    validateProviderEnvVarsDoNotCollide extProviders =
      envVarCollisions "server" (\extProvider -> extProvider.envVars.server)
        ++ envVarCollisions "client" (\extProvider -> extProvider.envVars.client)
      where
        envVarCollisions side getEnvVars =
          [ GenericValidationError $
              "Auth providers "
                ++ intercalate " and " (map (\ownerId -> "'" ++ ownerId ++ "'") ownerIds)
                ++ " both declare the "
                ++ side
                ++ " env var '"
                ++ envVarName
                ++ "'. Providers cannot share env var names: each provider reads its own configuration."
          | (envVarName, ownerIds) <- duplicatedVarNamesWithOwners
          ]
          where
            varNameOwnership =
              [ (envVar.name, extProvider.providerId)
              | extProvider <- extProviders,
                envVar <- getEnvVars extProvider
              ]
            duplicatedVarNamesWithOwners =
              [ (envVarName, snd <$> ownerships)
              | ownerships@((envVarName, _) : _ : _) <-
                  groupBy (\a b -> fst a == fst b) $ sortBy (\a b -> compare (fst a) (fst b)) varNameOwnership
              ]

-- | Every provider-restricted auth requirement (@authRequired: [...]@ on a
-- page, @auth: [...]@ on a query/action/api) must name configured provider
-- ids. Checked here rather than in the TS mapper because only the whole spec
-- knows the provider registry.
validateAuthRequirements :: AppSpec -> [ValidationError]
validateAuthRequirements spec =
  concatMap (uncurry validateRequirement) requirementSites
  where
    requirementSites =
      concat
        [ [ ("page '" ++ name ++ "' authRequired", requirement)
          | (name, page) <- AS.getPages spec,
            Just requirement <- [Page.authRequired page]
          ],
          [ ("query '" ++ name ++ "' auth", requirement)
          | (name, query) <- AS.getQueries spec,
            Just requirement <- [AS.Query.auth query]
          ],
          [ ("action '" ++ name ++ "' auth", requirement)
          | (name, action) <- AS.getActions spec,
            Just requirement <- [AS.Action.auth action]
          ],
          [ ("api '" ++ name ++ "' auth", requirement)
          | (name, api) <- AS.getApis spec,
            Just requirement <- [AS.Api.auth api]
          ]
        ]

    configuredProviderIds =
      maybe [] (map Auth.authProviderId . Auth.providers) (App.auth $ snd $ getApp spec)

    validateRequirement site requirement = case AuthRequirement.requiredAuthProviderIds requirement of
      Nothing -> []
      Just requirementProviderIds ->
        concat
          [ [ GenericValidationError $
                "Expected " ++ site ++ " to list at least one auth provider id (an empty list would let nobody in). Use false to disable auth instead."
            | null requirementProviderIds
            ],
            [ GenericValidationError $
                "Expected " ++ site ++ " to list each auth provider id at most once, but '" ++ duplicateId ++ "' appears more than once."
            | duplicateId <- findDuplicateElems requirementProviderIds
            ],
            [ GenericValidationError $
                "Expected "
                  ++ site
                  ++ " to list configured auth provider ids, but '"
                  ++ unknownId
                  ++ "' is not one. "
                  ++ if null configuredProviderIds
                    then "The app has no auth configured (app.auth is not set)."
                    else "Configured provider ids: " ++ intercalate ", " configuredProviderIds ++ "."
            | unknownId <- requirementProviderIds,
              unknownId `notElem` configuredProviderIds
            ]
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
    pageRequiresAuth page = AuthRequirement.isAuthRequiredWithDefault False (Page.authRequired page)

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

-- | Whether Wasp's own auth is among the app's auth providers. Everything
-- password-shaped (login routes, auth forms, wasp-auth method routes) is
-- generated only when this holds.
isWaspAuthUsed :: AppSpec -> Bool
isWaspAuthUsed spec = maybe False Auth.isWaspAuthProviderUsed (App.auth $ snd $ getApp spec)

getExternalAuthProviders :: AppSpec -> [Auth.ExternalAuthProviderSpec]
getExternalAuthProviders spec = maybe [] Auth.externalProviders (App.auth $ snd $ getApp spec)

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
