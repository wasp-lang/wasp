{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthProvider (..),
    WaspAuthConfig (..),
    AuthMethods (..),
    ExternalAuthConfig (..),
    EmailAuthConfig (..),
    UsernameAndPasswordConfig (..),
    ExternalAuthProviderSpec (..),
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
    externalProvider,
    isExternalAuthProviderUsed,
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
  )
where

import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Data (Data)
import Data.Maybe (isJust)
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
    provider :: AuthProvider
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | The authentication provider of the app, mirroring the discriminated union
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
    waspAuthOnBeforeSignup :: Maybe ExtImport,
    waspAuthOnAfterSignup :: Maybe ExtImport,
    waspAuthOnAfterEmailVerified :: Maybe ExtImport,
    waspAuthOnBeforeOAuthRedirect :: Maybe ExtImport,
    waspAuthOnBeforeLogin :: Maybe ExtImport,
    waspAuthOnAfterLogin :: Maybe ExtImport
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
  { -- | Stable identifier ("clerk", "better-auth"). Identities Wasp provisions
    -- for this provider's subjects are recorded under this name.
    providerId :: String,
    -- | Module specifier of an adapter package's server entry, e.g.
    -- @\@wasp.sh\/auth-clerk\/server@. Exactly one of 'serverPackage' and
    -- 'serverModule' is set; the mapper enforces it.
    serverPackage :: Maybe String,
    -- | User-code module implementing the provider (the hand-written adapter
    -- escape hatch), reached through a virtual user module like every other
    -- ext import.
    serverModule :: Maybe ExtImport,
    -- | Module specifier of an adapter package's client entry.
    clientPackage :: Maybe String,
    -- | Routes the provider wants mounted on Wasp's server, for providers that
    -- own HTTP endpoints of their own (Better Auth).
    routes :: Maybe ExternalProviderRoutes,
    capabilities :: [String],
    envVars :: ExternalProviderEnvVars,
    -- | Populates the user entity when Wasp provisions a local user for a
    -- subject it has not seen before.
    userSignupFields :: Maybe ExtImport,
    -- | Setup function for the provider's underlying library (the
    -- @prismaSetupFn@ convention): the adapter calls it with its integration
    -- config and uses the returned configuration. The escape hatch for
    -- everything serializable options cannot carry.
    setupFn :: Maybe ExtImport,
    -- | The adapter's serializable options, JSON-encoded. Kept as a string so
    -- the generator can splice it into generated code verbatim.
    optionsJson :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

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
    doc :: Maybe String
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

-- Accessors over the provider sum, with the signatures consumers always had:
-- wasp-auth-only configuration simply reads as absent under an external
-- provider, which is exactly what it is.

methods :: Auth -> AuthMethods
methods auth = case provider auth of
  WaspAuthProvider config -> waspAuthMethods config
  ExternalAuthProvider _ -> emptyAuthMethods

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
withWaspAuthConfig getField auth = case provider auth of
  WaspAuthProvider config -> getField config
  ExternalAuthProvider _ -> Nothing

onAuthSucceededRedirectTo :: Auth -> Maybe String
onAuthSucceededRedirectTo = withWaspAuthConfig waspAuthOnAuthSucceededRedirectTo

onBeforeSignup :: Auth -> Maybe ExtImport
onBeforeSignup = withWaspAuthConfig waspAuthOnBeforeSignup

onAfterSignup :: Auth -> Maybe ExtImport
onAfterSignup = withWaspAuthConfig waspAuthOnAfterSignup

onAfterEmailVerified :: Auth -> Maybe ExtImport
onAfterEmailVerified = withWaspAuthConfig waspAuthOnAfterEmailVerified

onBeforeOAuthRedirect :: Auth -> Maybe ExtImport
onBeforeOAuthRedirect = withWaspAuthConfig waspAuthOnBeforeOAuthRedirect

onBeforeLogin :: Auth -> Maybe ExtImport
onBeforeLogin = withWaspAuthConfig waspAuthOnBeforeLogin

onAfterLogin :: Auth -> Maybe ExtImport
onAfterLogin = withWaspAuthConfig waspAuthOnAfterLogin

externalProvider :: Auth -> Maybe ExternalAuthProviderSpec
externalProvider auth = case provider auth of
  WaspAuthProvider _ -> Nothing
  ExternalAuthProvider extProvider -> Just extProvider

isExternalAuthProviderUsed :: Auth -> Bool
isExternalAuthProviderUsed = isJust . externalProvider

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
