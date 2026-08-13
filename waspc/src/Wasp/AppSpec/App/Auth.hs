{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthMethods (..),
    ExternalAuthConfig (..),
    EmailAuthConfig (..),
    UsernameAndPasswordConfig (..),
    ExternalAuthProviderSpec (..),
    ExternalProviderRoutes (..),
    ExternalProviderEnvVars (..),
    ExternalProviderEnvVar (..),
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

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth.EmailVerification (EmailVerificationConfig)
import Wasp.AppSpec.App.Auth.PasswordReset (PasswordResetConfig)
import Wasp.AppSpec.App.EmailSender (EmailFromField)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Auth = Auth
  { userEntity :: Ref Entity,
    methods :: AuthMethods,
    onAuthFailedRedirectTo :: String,
    onAuthSucceededRedirectTo :: Maybe String,
    onBeforeSignup :: Maybe ExtImport,
    onAfterSignup :: Maybe ExtImport,
    onAfterEmailVerified :: Maybe ExtImport,
    onBeforeOAuthRedirect :: Maybe ExtImport,
    onBeforeLogin :: Maybe ExtImport,
    onAfterLogin :: Maybe ExtImport,
    -- | EXPERIMENTAL. The external provider that verifies incoming requests.
    --
    -- 'Nothing' means Wasp's own auth, which is what 'methods' configures.
    --
    -- In the user-facing spec, @auth.provider@ is a discriminated union: either
    -- @waspAuth({ methods, hooks, ... })@ or an external provider manifest. The
    -- spec mapper normalizes that union into this flat record, guaranteeing by
    -- construction that 'methods' is empty and every hook is 'Nothing' whenever
    -- 'externalProvider' is set. The IR stays flat because the classic
    -- Analyzer's Template Haskell cannot derive declarations for sum types.
    externalProvider :: Maybe ExternalAuthProviderSpec
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

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
    -- | Escape hatch for non-serializable adapter configuration (functions,
    -- class instances). Applied by the adapter's server factory after its own
    -- defaults.
    extendServerConfig :: Maybe ExtImport,
    -- | The adapter's serializable options, JSON-encoded. Kept as a string so
    -- the IR stays Template-Haskell-friendly; the generator splices it into
    -- generated code verbatim.
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

isExternalAuthProviderUsed :: Auth -> Bool
isExternalAuthProviderUsed = isJust . externalProvider

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
