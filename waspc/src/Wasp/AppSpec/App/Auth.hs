{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthHooksSpec (..),
    AuthProvider (..),
    WaspAuthConfig (..),
    AuthMethods (..),
    ExternalAuthConfig (..),
    EmailAuthConfig (..),
    UsernameAndPasswordConfig (..),
    ExternalAuthProviderSpec (..),
    ExternalProviderServer (..),
    ExternalProviderRoutes (..),
    ExternalProviderEnvVars (..),
    ExternalProviderEnvVar (..),
    methods,
    onAuthSucceededRedirectTo,
    onBeforeSignup,
    onAfterSignup,
    onAfterEmailVerified,
    onBeforeOAuthRedirect,
    onBeforeLogin,
    onAfterLogin,
    waspAuthConfig,
    externalProviders,
    authProviderId,
    waspAuthProviderId,
    serverPackage,
    serverModule,
    isWaspAuthProviderUsed,
    isExternalAuthProviderUsed,
    isClientAuthAdapterUsed,
    isUsernameAndPasswordAuthEnabled,
    isExternalAuthEnabled,
    isSlackAuthEnabled,
    isDiscordAuthEnabled,
    isGoogleAuthEnabled,
    isKeycloakAuthEnabled,
    isGitHubAuthEnabled,
    isMicrosoftAuthEnabled,
    isEmailAuthEnabled,
    enabledAuthMethodNames,
    userSignupFieldsForEmailAuth,
    userSignupFieldsForUsernameAuth,
    userSignupFieldsForExternalAuth,
    userSignupFieldsForExternalAuthProvider,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Data (Data)
import Data.Maybe (isJust, listToMaybe)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth.EmailVerification (EmailVerificationConfig)
import Wasp.AppSpec.App.Auth.PasswordReset (PasswordResetConfig)
import Wasp.AppSpec.App.EmailSender (EmailFromField)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.Util (toLowerFirst)

data Auth = Auth
  { userEntity :: Ref Entity,
    onAuthFailedRedirectTo :: String,
    -- | The app's auth providers, in declaration order. Validation guarantees
    -- the list is non-empty, provider ids are pairwise distinct, and at most
    -- one element is a 'WaspAuthProvider'.
    providers :: [AuthProvider],
    -- | App-level lifecycle hooks, fired at Wasp-owned choke points (identity
    -- provisioning, session minting) for EVERY provider -- an adapter can
    -- neither forget nor forge them. Method-specific hooks
    -- (onAfterEmailVerified, onBeforeOAuthRedirect) stay wasp-auth config.
    hooks :: Maybe AuthHooksSpec
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | The app's generic auth lifecycle hooks. Field names carry a @hooks@
-- prefix so this module can export the natural accessor names; the JSON
-- representation uses the natural names (see the aeson options below).
data AuthHooksSpec = AuthHooksSpec
  { hooksOnBeforeSignup :: Maybe ExtImport,
    hooksOnAfterSignup :: Maybe ExtImport,
    hooksOnBeforeLogin :: Maybe ExtImport,
    hooksOnAfterLogin :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic)

authHooksSpecJsonOptions :: Aeson.Options
authHooksSpecJsonOptions =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = toLowerFirst . drop (length ("hooks" :: String))}

instance FromJSON AuthHooksSpec where
  parseJSON = Aeson.genericParseJSON authHooksSpecJsonOptions

instance ToJSON AuthHooksSpec where
  toJSON = Aeson.genericToJSON authHooksSpecJsonOptions

-- | One authentication provider of the app, mirroring the discriminated union
-- in the user-facing spec: either Wasp's own auth, carrying everything that
-- only makes sense when Wasp runs the signup and login flows, or an external
-- provider's manifest. The impossible states -- auth methods next to Clerk,
-- Wasp's hooks next to a manifest -- are unrepresentable.
--
-- NOTE: the classic Analyzer's Template Haskell cannot derive declarations for
-- sum types, so this type has a 'HasCustomEvaluation' instance
-- ("Wasp.Analyzer.StdTypeDefinitions.App.AuthProvider"). The classic wasp DSL
-- cannot express providers at all, so that evaluation only reports as much.
data AuthProvider
  = WaspAuthProvider WaspAuthConfig
  | ExternalAuthProvider ExternalAuthProviderSpec
  deriving (Show, Eq, Data, Generic)

instance FromJSON AuthProvider where
  parseJSON = Aeson.withObject "AuthProvider" $ \obj ->
    (obj .: "kind") >>= \case
      "wasp" -> WaspAuthProvider <$> Aeson.parseJSON (Aeson.Object obj)
      "external" -> ExternalAuthProvider <$> Aeson.parseJSON (Aeson.Object obj)
      (unknownKind :: String) -> fail $ "Unknown auth provider kind: " ++ unknownKind

instance ToJSON AuthProvider where
  toJSON = \case
    WaspAuthProvider config -> injectKind "wasp" $ Aeson.toJSON config
    ExternalAuthProvider extProvider -> injectKind "external" $ Aeson.toJSON extProvider
    where
      injectKind :: String -> Aeson.Value -> Aeson.Value
      injectKind kind (Aeson.Object obj) = Aeson.Object $ KeyMap.insert "kind" (Aeson.toJSON kind) obj
      injectKind _ value = value

-- | Configuration of Wasp's own auth.
--
-- Fields carry a @waspAuth@ prefix so this module can export 'Auth'-level
-- accessors under the natural names ('methods', 'onBeforeSignup', ...); the
-- JSON representation uses the natural names (see the aeson options below).
data WaspAuthConfig = WaspAuthConfig
  { waspAuthMethods :: AuthMethods,
    waspAuthOnAuthSucceededRedirectTo :: Maybe String,
    waspAuthOnAfterEmailVerified :: Maybe ExtImport,
    waspAuthOnBeforeOAuthRedirect :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic)

waspAuthConfigJsonOptions :: Aeson.Options
waspAuthConfigJsonOptions =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = toLowerFirst . drop (length ("waspAuth" :: String))}

instance FromJSON WaspAuthConfig where
  parseJSON = Aeson.genericParseJSON waspAuthConfigJsonOptions

instance ToJSON WaspAuthConfig where
  toJSON = Aeson.genericToJSON waspAuthConfigJsonOptions

-- | An external auth provider, as declared by its manifest in the spec.
--
-- 'capabilities' is deliberately an open set of strings: adapters ship
-- independently of Wasp releases, so a closed enum here would break decoding of
-- every manifest built against a newer adapter. Unknown entries are ignored.
data ExternalAuthProviderSpec = ExternalAuthProviderSpec
  { -- | Stable identifier ("external:clerk", "external:better-auth").
    -- Identities Wasp provisions for this provider's subjects are recorded
    -- under this name.
    providerId :: String,
    -- | Where the provider's implementation comes from.
    server :: ExternalProviderServer,
    -- | Module specifier of an adapter package's client entry, if it has one.
    clientPackage :: Maybe String,
    -- | Routes the provider wants mounted on Wasp's server, for providers that
    -- own HTTP endpoints of their own (Better Auth).
    routes :: Maybe ExternalProviderRoutes,
    capabilities :: [String],
    envVars :: ExternalProviderEnvVars,
    -- | Runtime facets the adapter requests from Wasp ("wasp-sessions",
    -- "email-send", "identity-namespaces"). Validation rejects unknown names:
    -- the generator can only wire facets it knows.
    uses :: [String],
    -- | Identity namespaces this provider records identities under. Defaults
    -- to @[providerId]@; extras must be @providerId ++ "/" ++ suffix@.
    identityNamespaces :: [String],
    -- | Populates the user entity when Wasp provisions a local user for a
    -- subject it has not seen before.
    userSignupFields :: Maybe ExtImport,
    -- | Setup function for the provider's underlying library.
    setupFn :: Maybe ExtImport,
    -- | The adapter's serializable options, JSON-encoded.
    optionsJson :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Exactly one of: an adapter package's server entry (module specifier), or a
-- user-code module implementing the provider (the hand-written escape hatch).
newtype ExternalProviderServer = ExternalProviderServer (Either String ExtImport)
  deriving (Show, Eq, Data, Generic)

instance FromJSON ExternalProviderServer where
  parseJSON = Aeson.withObject "server" $ \o -> do
    maybePackage <- o Aeson..:? "package"
    maybeModule <- o Aeson..:? "module"
    case (maybePackage, maybeModule) of
      (Just packageSpecifier, Nothing) -> pure $ ExternalProviderServer (Left packageSpecifier)
      (Nothing, Just extImport) -> pure $ ExternalProviderServer (Right extImport)
      _ -> fail "server must contain exactly one of 'package' and 'module'"

instance ToJSON ExternalProviderServer where
  toJSON (ExternalProviderServer (Left packageSpecifier)) =
    Aeson.object ["package" Aeson..= packageSpecifier]
  toJSON (ExternalProviderServer (Right extImport)) =
    Aeson.object ["module" Aeson..= extImport]

serverPackage :: ExternalAuthProviderSpec -> Maybe String
serverPackage spec = case spec.server of
  ExternalProviderServer (Left packageSpecifier) -> Just packageSpecifier
  ExternalProviderServer (Right _) -> Nothing

serverModule :: ExternalAuthProviderSpec -> Maybe ExtImport
serverModule spec = case spec.server of
  ExternalProviderServer (Left _) -> Nothing
  ExternalProviderServer (Right extImport) -> Just extImport

-- | Whether any configured external provider brings a client-side adapter
-- entry.
isClientAuthAdapterUsed :: Auth -> Bool
isClientAuthAdapterUsed = any (isJust . clientPackage) . externalProviders

data ExternalProviderRoutes = ExternalProviderRoutes
  { basePath :: String,
    -- | When true, the provider's routes are mounted without the JSON body
    -- parser, because the provider reads the raw request body itself.
    rawBody :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data ExternalProviderEnvVars = ExternalProviderEnvVars
  { server :: [ExternalProviderEnvVar],
    client :: [ExternalProviderEnvVar]
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data ExternalProviderEnvVar = ExternalProviderEnvVar
  { name :: String,
    optional :: Maybe Bool,
    doc :: Maybe String,
    -- | Development fallback value: applied when the var is unset in dev, so
    -- @wasp start@ works out of the box; production keeps the var required.
    devDefault :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data AuthMethods = AuthMethods
  { usernameAndPassword :: Maybe UsernameAndPasswordConfig,
    slack :: Maybe ExternalAuthConfig,
    discord :: Maybe ExternalAuthConfig,
    google :: Maybe ExternalAuthConfig,
    gitHub :: Maybe ExternalAuthConfig,
    keycloak :: Maybe ExternalAuthConfig,
    microsoft :: Maybe ExternalAuthConfig,
    email :: Maybe EmailAuthConfig
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data UsernameAndPasswordConfig = UsernameAndPasswordConfig
  { userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data ExternalAuthConfig = ExternalAuthConfig
  { configFn :: Maybe ExtImport,
    userSignupFields :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data EmailAuthConfig = EmailAuthConfig
  { userSignupFields :: Maybe ExtImport,
    fromField :: EmailFromField,
    emailVerification :: EmailVerificationConfig,
    passwordReset :: PasswordResetConfig
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- Accessors over the providers list, with the signatures consumers always
-- had: wasp-auth-only configuration simply reads as absent when no
-- 'WaspAuthProvider' is among the providers, which is exactly what it is.

-- | The registry id of a provider: @"wasp"@ for Wasp's own auth, the manifest
-- id (@"external:clerk"@) for external providers. This single id addresses the
-- provider everywhere: the exchange route, the Session row, the identity
-- store, and the generated registries.
authProviderId :: AuthProvider -> String
authProviderId (WaspAuthProvider _) = waspAuthProviderId
authProviderId (ExternalAuthProvider extProvider) = providerId extProvider

waspAuthProviderId :: String
waspAuthProviderId = "wasp"

waspAuthConfig :: Auth -> Maybe WaspAuthConfig
waspAuthConfig auth = listToMaybe [config | WaspAuthProvider config <- providers auth]

methods :: Auth -> AuthMethods
methods = maybe emptyAuthMethods waspAuthMethods . waspAuthConfig

emptyAuthMethods :: AuthMethods
emptyAuthMethods =
  AuthMethods
    { usernameAndPassword = Nothing,
      slack = Nothing,
      discord = Nothing,
      google = Nothing,
      gitHub = Nothing,
      keycloak = Nothing,
      microsoft = Nothing,
      email = Nothing
    }

withWaspAuthConfig :: (WaspAuthConfig -> Maybe a) -> Auth -> Maybe a
withWaspAuthConfig getField auth = waspAuthConfig auth >>= getField

onAuthSucceededRedirectTo :: Auth -> Maybe String
onAuthSucceededRedirectTo = withWaspAuthConfig waspAuthOnAuthSucceededRedirectTo

onBeforeSignup :: Auth -> Maybe ExtImport
onBeforeSignup auth = hooks auth >>= hooksOnBeforeSignup

onAfterSignup :: Auth -> Maybe ExtImport
onAfterSignup auth = hooks auth >>= hooksOnAfterSignup

onAfterEmailVerified :: Auth -> Maybe ExtImport
onAfterEmailVerified = withWaspAuthConfig waspAuthOnAfterEmailVerified

onBeforeOAuthRedirect :: Auth -> Maybe ExtImport
onBeforeOAuthRedirect = withWaspAuthConfig waspAuthOnBeforeOAuthRedirect

onBeforeLogin :: Auth -> Maybe ExtImport
onBeforeLogin auth = hooks auth >>= hooksOnBeforeLogin

onAfterLogin :: Auth -> Maybe ExtImport
onAfterLogin auth = hooks auth >>= hooksOnAfterLogin

-- | The external providers among the app's providers, in declaration order.
externalProviders :: Auth -> [ExternalAuthProviderSpec]
externalProviders auth = [extProvider | ExternalAuthProvider extProvider <- providers auth]

isWaspAuthProviderUsed :: Auth -> Bool
isWaspAuthProviderUsed = isJust . waspAuthConfig

isExternalAuthProviderUsed :: Auth -> Bool
isExternalAuthProviderUsed = not . null . externalProviders

isUsernameAndPasswordAuthEnabled :: Auth -> Bool
isUsernameAndPasswordAuthEnabled = isJust . usernameAndPassword . methods

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth =
  any
    ($ auth)
    -- NOTE: Make sure to add new external auth methods here.
    [ isSlackAuthEnabled,
      isDiscordAuthEnabled,
      isGoogleAuthEnabled,
      isGitHubAuthEnabled,
      isKeycloakAuthEnabled,
      isMicrosoftAuthEnabled
    ]

isSlackAuthEnabled :: Auth -> Bool
isSlackAuthEnabled = isJust . slack . methods

isDiscordAuthEnabled :: Auth -> Bool
isDiscordAuthEnabled = isJust . discord . methods

isGoogleAuthEnabled :: Auth -> Bool
isGoogleAuthEnabled = isJust . google . methods

isKeycloakAuthEnabled :: Auth -> Bool
isKeycloakAuthEnabled = isJust . keycloak . methods

isGitHubAuthEnabled :: Auth -> Bool
isGitHubAuthEnabled = isJust . gitHub . methods

isMicrosoftAuthEnabled :: Auth -> Bool
isMicrosoftAuthEnabled = isJust . microsoft . methods

isEmailAuthEnabled :: Auth -> Bool
isEmailAuthEnabled = isJust . email . methods

-- | Names of the auth methods enabled in the app, as the user knows them.
enabledAuthMethodNames :: AuthMethods -> [String]
enabledAuthMethodNames authMethods =
  [ methodName
  | (methodName, isEnabled) <-
      -- NOTE: Make sure to add new auth methods here.
      [ ("usernameAndPassword", isJust $ usernameAndPassword authMethods),
        ("slack", isJust $ slack authMethods),
        ("discord", isJust $ discord authMethods),
        ("google", isJust $ google authMethods),
        ("gitHub", isJust $ gitHub authMethods),
        ("keycloak", isJust $ keycloak authMethods),
        ("microsoft", isJust $ microsoft authMethods),
        ("email", isJust $ email authMethods)
      ],
    isEnabled
  ]

-- These helper functions are used to avoid ambiguity when using the
-- `userSignupFields` function (otherwise we need to use DuplicateRecordFields
-- and OverloadedRecordDot extension in each module that uses them).
userSignupFieldsForEmailAuth :: EmailAuthConfig -> Maybe ExtImport
userSignupFieldsForEmailAuth = (.userSignupFields)

userSignupFieldsForUsernameAuth :: UsernameAndPasswordConfig -> Maybe ExtImport
userSignupFieldsForUsernameAuth = (.userSignupFields)

userSignupFieldsForExternalAuth :: ExternalAuthConfig -> Maybe ExtImport
userSignupFieldsForExternalAuth = (.userSignupFields)

userSignupFieldsForExternalAuthProvider :: ExternalAuthProviderSpec -> Maybe ExtImport
userSignupFieldsForExternalAuthProvider = (.userSignupFields)
