{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthHooksSpec (..),
    AuthProvider,
    AuthProviderSpec (..),
    AuthProviderServer (..),
    AuthProviderRoutes (..),
    AuthProviderEnvVars (..),
    AuthProviderEnvVar (..),
    onBeforeSignup,
    onAfterSignup,
    onBeforeLogin,
    onAfterLogin,
    serverPackage,
    serverModule,
    isClientAuthAdapterUsed,
    userSignupFieldsForAuthProvider,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Map (Map)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.Util (toLowerFirst)

data Auth = Auth
  { userEntity :: Ref Entity,
    onAuthFailedRedirectTo :: String,
    -- | The app's auth providers, in declaration order. Every provider is an
    -- adapter package's manifest -- Wasp's own auth included. Validation
    -- guarantees the list is non-empty and provider ids are pairwise distinct.
    providers :: [AuthProvider],
    -- | App-level lifecycle hooks, fired at Wasp-owned choke points (identity
    -- provisioning, session minting) for EVERY provider -- an adapter can
    -- neither forget nor forge them. Method-specific hooks belong to the
    -- provider package that implements the method.
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

-- | One authentication provider of the app: an adapter package's manifest, as
-- declared in the spec. The classic wasp DSL cannot express providers at all
-- (they are constructed by spec helpers like @waspAuth()@, which only exist
-- in the TypeScript spec), so "Wasp.Analyzer.StdTypeDefinitions.App.AuthProvider"
-- reports as much.
type AuthProvider = AuthProviderSpec

-- | An auth provider, as declared by its manifest in the spec.
--
-- 'capabilities' is deliberately an open set of strings: adapters ship
-- independently of Wasp releases, so a closed enum here would break decoding of
-- every manifest built against a newer adapter. Unknown entries are ignored.
data AuthProviderSpec = AuthProviderSpec
  { -- | Stable identifier ("wasp", "clerk", "better-auth"). Identities Wasp
    -- provisions for this provider's subjects are recorded under this name
    -- (or under one of its 'identityNamespaces'), and sessions record it as
    -- their minting provider.
    providerId :: String,
    -- | Where the provider's implementation comes from.
    server :: AuthProviderServer,
    -- | Module specifier of an adapter package's client entry, if it has one.
    clientPackage :: Maybe String,
    -- | Routes the provider wants mounted on Wasp's server, for providers that
    -- own HTTP endpoints of their own (Wasp's own auth, Better Auth).
    routes :: Maybe AuthProviderRoutes,
    capabilities :: [String],
    envVars :: AuthProviderEnvVars,
    -- | Runtime facets the adapter requests from Wasp ("wasp-sessions",
    -- "email-send", "identity-namespaces"). Validation rejects unknown names:
    -- the generator can only wire facets it knows.
    uses :: [String],
    -- | Identity namespaces this provider records identities under. Defaults
    -- to @[providerId]@; extras must be @providerId ++ ":" ++ suffix@.
    identityNamespaces :: [String],
    -- | Populates the user entity when Wasp provisions a local user for a
    -- subject it has not seen before.
    userSignupFields :: Maybe ExtImport,
    -- | Setup function for the provider's underlying library.
    setupFn :: Maybe ExtImport,
    -- | Every other user function the adapter calls back into, keyed by the
    -- name the adapter expects. Delivered to the adapter's server factory
    -- through virtual user modules, like every other user function.
    extensions :: Map String ExtImport,
    -- | The adapter's serializable options, JSON-encoded.
    optionsJson :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Exactly one of: an adapter package's server entry (module specifier), or a
-- user-code module implementing the provider (the hand-written escape hatch).
newtype AuthProviderServer = AuthProviderServer (Either String ExtImport)
  deriving (Show, Eq, Data, Generic)

instance FromJSON AuthProviderServer where
  parseJSON = Aeson.withObject "server" $ \o -> do
    maybePackage <- o Aeson..:? "package"
    maybeModule <- o Aeson..:? "module"
    case (maybePackage, maybeModule) of
      (Just packageSpecifier, Nothing) -> pure $ AuthProviderServer (Left packageSpecifier)
      (Nothing, Just extImport) -> pure $ AuthProviderServer (Right extImport)
      _ -> fail "server must contain exactly one of 'package' and 'module'"

instance ToJSON AuthProviderServer where
  toJSON (AuthProviderServer (Left packageSpecifier)) =
    Aeson.object ["package" Aeson..= packageSpecifier]
  toJSON (AuthProviderServer (Right extImport)) =
    Aeson.object ["module" Aeson..= extImport]

serverPackage :: AuthProviderSpec -> Maybe String
serverPackage spec = case spec.server of
  AuthProviderServer (Left packageSpecifier) -> Just packageSpecifier
  AuthProviderServer (Right _) -> Nothing

serverModule :: AuthProviderSpec -> Maybe ExtImport
serverModule spec = case spec.server of
  AuthProviderServer (Left _) -> Nothing
  AuthProviderServer (Right extImport) -> Just extImport

-- | Whether any configured provider brings a client-side adapter entry.
isClientAuthAdapterUsed :: Auth -> Bool
isClientAuthAdapterUsed = any (isJust . clientPackage) . providers

data AuthProviderRoutes = AuthProviderRoutes
  { basePath :: String,
    -- | When true, the provider's routes are mounted without the JSON body
    -- parser, because the provider reads the raw request body itself.
    rawBody :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data AuthProviderEnvVars = AuthProviderEnvVars
  { server :: [AuthProviderEnvVar],
    client :: [AuthProviderEnvVar]
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data AuthProviderEnvVar = AuthProviderEnvVar
  { name :: String,
    optional :: Maybe Bool,
    doc :: Maybe String,
    -- | Development fallback value: applied when the var is unset in dev, so
    -- @wasp start@ works out of the box; production keeps the var required.
    devDefault :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

onBeforeSignup :: Auth -> Maybe ExtImport
onBeforeSignup auth = hooks auth >>= hooksOnBeforeSignup

onAfterSignup :: Auth -> Maybe ExtImport
onAfterSignup auth = hooks auth >>= hooksOnAfterSignup

onBeforeLogin :: Auth -> Maybe ExtImport
onBeforeLogin auth = hooks auth >>= hooksOnBeforeLogin

onAfterLogin :: Auth -> Maybe ExtImport
onAfterLogin auth = hooks auth >>= hooksOnAfterLogin

-- Avoids ambiguity with the other `userSignupFields` record fields (otherwise
-- every consumer would need DuplicateRecordFields and OverloadedRecordDot).
userSignupFieldsForAuthProvider :: AuthProviderSpec -> Maybe ExtImport
userSignupFieldsForAuthProvider = (.userSignupFields)
