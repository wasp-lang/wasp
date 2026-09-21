{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthHooksSpec (..),
    AuthScheme (..),
    AuthSchemeSide (..),
    AuthAdapterEntry (..),
    AuthSchemeRoutes (..),
    AuthSchemeEnvVar (..),
    AuthSchemeProvider (..),
    providerNames,
    AuthSchemeCredentials (..),
    CredentialTransport (..),
    CredentialStore (..),
    onBeforeSignup,
    onAfterSignup,
    onBeforeLogin,
    onAfterLogin,
    onBeforeLink,
    onAfterLink,
    serverPackage,
    serverModule,
    clientPackage,
    clientModule,
    serverEnvVars,
    clientEnvVars,
    serverExportName,
    clientExportName,
    credentialsScheme,
    inlineCredentials,
    canSignIn,
    isCookieTransportUsed,
    isClientAuthHandlerUsed,
    schemeNames,
  )
where

import Data.Aeson (FromJSON, ToJSON, (.:), (.:?), (.=))
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
    -- | The app's auth schemes, in declaration order: named, configured
    -- instances of auth handlers. Validation guarantees the list is non-empty
    -- and names are pairwise distinct.
    schemes :: [AuthScheme],
    -- | The scheme that authenticates assets which only say
    -- @authRequired: true@. The mapper resolves it: the one scheme when there
    -- is one, the declared @default@ otherwise.
    defaultScheme :: String,
    -- | App-level lifecycle hooks, fired at Wasp-owned choke points (identity
    -- provisioning, credential issuance) for EVERY scheme -- a handler can
    -- neither forget nor forge them. Method-specific hooks belong to the
    -- handler package that implements the method.
    hooks :: Maybe AuthHooksSpec,
    -- | Turns account merging on: the app's function for combining two users'
    -- data. App-level, because it concerns the user entity, not one handler.
    mergeUsers :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | The app's generic auth lifecycle hooks. Field names carry a @hooks@
-- prefix so this module can export the natural accessor names; the JSON
-- representation uses the natural names (see the aeson options below).
data AuthHooksSpec = AuthHooksSpec
  { hooksOnBeforeSignup :: Maybe ExtImport,
    hooksOnAfterSignup :: Maybe ExtImport,
    hooksOnBeforeLogin :: Maybe ExtImport,
    hooksOnAfterLogin :: Maybe ExtImport,
    hooksOnBeforeLink :: Maybe ExtImport,
    hooksOnAfterLink :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic)

authHooksSpecJsonOptions :: Aeson.Options
authHooksSpecJsonOptions =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = toLowerFirst . drop (length ("hooks" :: String))}

instance FromJSON AuthHooksSpec where
  parseJSON = Aeson.genericParseJSON authHooksSpecJsonOptions

instance ToJSON AuthHooksSpec where
  toJSON = Aeson.genericToJSON authHooksSpecJsonOptions

-- | A named, configured instance of an auth handler, as declared in the spec.
--
-- 'capabilities' is deliberately an open set of strings: handlers ship
-- independently of Wasp releases, so a closed enum here would break decoding of
-- every manifest built against a newer handler. Unknown entries are ignored.
data AuthScheme = AuthScheme
  { -- | The scheme name: the key in @auth.schemes@. Sessions record it,
    -- provider names and routes are prefixed with it, and
    -- @authRequired@ lists name it.
    name :: String,
    -- | A label for the handler, for messages: where its server half's code
    -- lives (a package specifier, or the path of a hand-written adapter).
    handler :: String,
    -- | The scheme's server half.
    server :: AuthSchemeSide,
    -- | The scheme's client half, if it has one.
    client :: Maybe AuthSchemeSide,
    -- | Whether the handler brings its own routes, mounted at @/auth/<name>@.
    routes :: Maybe AuthSchemeRoutes,
    capabilities :: [String],
    -- | Runtime facets the handler requests from Wasp ("email-send",
    -- so far). Validation rejects unknown names: the generator
    -- can only wire facets it knows.
    uses :: [String],
    -- | The providers this scheme records identities under, named as stored
    -- in @AuthIdentity.providerName@ (@"email"@); just @"default"@ when the
    -- manifest declared none. The scheme's name goes to the @handlerName@
    -- column next to it.
    providers :: [AuthSchemeProvider],
    -- | How the scheme hands out credentials after a login it verified;
    -- absent for schemes whose own credential authenticates every request.
    credentials :: Maybe AuthSchemeCredentials,
    -- | Computes the user entity's fields from verified claims, for the one
    -- case where WASP creates the user: a subject it has never seen shows
    -- up already authenticated. Wasp itself calls it, which is why it is not
    -- part of a side's spec.
    userFieldsFromClaims :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Exactly one of: a handler package's server entry (module specifier), or a
-- user-code module implementing the handler (the hand-written escape hatch).
-- | One half of a scheme (server or client): the adapter Wasp calls to build
-- it, and what that adapter receives.
data AuthSchemeSide = AuthSchemeSide
  { authAdapter :: AuthAdapterEntry,
    -- | Env vars this half reads; it receives exactly these.
    envVars :: [AuthSchemeEnvVar],
    -- | The plain-data part of this half's @spec@, JSON-encoded.
    specJson :: Maybe String,
    -- | The references to app code lifted out of this half's @spec@,
    -- keyed by the JSON-encoded path they sat at
    -- (@["methods","google","configFn"]@). The generated code imports each
    -- and sets it back at its path before calling the adapter.
    specReferences :: Map String ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Where a side's adapter lives. Both forms are the same thing in different
-- places, which is what makes a hand-written handler as powerful as a
-- packaged one: a handler package's entry and the name it exports the
-- adapter under, or an adapter in the app's own code.
data AuthAdapterEntry
  = PackageAdapter {packageSpecifier :: String, exportName :: String}
  | ModuleAdapter ExtImport
  deriving (Show, Eq, Data, Generic)

instance FromJSON AuthAdapterEntry where
  parseJSON = Aeson.withObject "authAdapter" $ \o -> do
    maybePackage <- o .:? "package"
    maybeExport <- o .:? "export"
    maybeModule <- o .:? "module"
    case (maybePackage, maybeExport, maybeModule) of
      (Just packageSpecifier', Just exportName', Nothing) ->
        pure $ PackageAdapter packageSpecifier' exportName'
      (Nothing, Nothing, Just extImport) -> pure $ ModuleAdapter extImport
      _ -> fail "authAdapter must be either { package, export } or { module }"

instance ToJSON AuthAdapterEntry where
  toJSON (PackageAdapter packageSpecifier' exportName') =
    Aeson.object ["package" .= packageSpecifier', "export" .= exportName']
  toJSON (ModuleAdapter extImport) =
    Aeson.object ["module" .= extImport]

adapterPackage :: AuthAdapterEntry -> Maybe String
adapterPackage (PackageAdapter packageSpecifier' _) = Just packageSpecifier'
adapterPackage (ModuleAdapter _) = Nothing

adapterModule :: AuthAdapterEntry -> Maybe ExtImport
adapterModule (PackageAdapter _ _) = Nothing
adapterModule (ModuleAdapter extImport) = Just extImport

serverPackage :: AuthScheme -> Maybe String
serverPackage scheme = adapterPackage scheme.server.authAdapter

serverModule :: AuthScheme -> Maybe ExtImport
serverModule scheme = adapterModule scheme.server.authAdapter

clientPackage :: AuthScheme -> Maybe String
clientPackage scheme = scheme.client >>= adapterPackage . (.authAdapter)

-- | The name a handler package exports its server adapter under. Nothing for
-- an adapter in the app's own code, whose reference already names its export.
serverExportName :: AuthScheme -> Maybe String
serverExportName scheme = adapterExportName scheme.server.authAdapter

clientExportName :: AuthScheme -> Maybe String
clientExportName scheme = scheme.client >>= adapterExportName . (.authAdapter)

adapterExportName :: AuthAdapterEntry -> Maybe String
adapterExportName (PackageAdapter _ exportName') = Just exportName'
adapterExportName (ModuleAdapter _) = Nothing

serverEnvVars :: AuthScheme -> [AuthSchemeEnvVar]
serverEnvVars scheme = scheme.server.envVars

-- | Empty for a scheme with no client half.
clientEnvVars :: AuthScheme -> [AuthSchemeEnvVar]
clientEnvVars scheme = maybe [] (.envVars) scheme.client

clientModule :: AuthScheme -> Maybe ExtImport
clientModule scheme = scheme.client >>= adapterModule . (.authAdapter)

-- | How a scheme hands out credentials: by signing into a sibling scheme, or
-- through a private Wasp issuer configured inline.
data AuthSchemeCredentials
  = CredentialsFromScheme String
  | InlineCredentials
      { transport :: CredentialTransport,
        store :: CredentialStore,
        -- | Credential lifetime, e.g. @"30d"@.
        ttl :: String
      }
  deriving (Show, Eq, Data, Generic)

instance FromJSON AuthSchemeCredentials where
  parseJSON = Aeson.withObject "credentials" $ \o -> do
    maybeScheme <- o .:? "scheme"
    case maybeScheme of
      Just schemeName -> pure $ CredentialsFromScheme schemeName
      Nothing ->
        InlineCredentials
          <$> o .: "transport"
          <*> o .: "store"
          <*> o .: "ttl"

instance ToJSON AuthSchemeCredentials where
  toJSON (CredentialsFromScheme schemeName) = Aeson.object ["scheme" .= schemeName]
  toJSON (InlineCredentials transport' store' ttl') =
    Aeson.object ["transport" .= transport', "store" .= store', "ttl" .= ttl']

data CredentialTransport = BearerTransport | CookieTransport
  deriving (Show, Eq, Data, Generic)

instance FromJSON CredentialTransport where
  parseJSON = Aeson.withText "transport" $ \case
    "bearer" -> pure BearerTransport
    "cookie" -> pure CookieTransport
    other -> fail $ "Unknown credential transport: " ++ show other

instance ToJSON CredentialTransport where
  toJSON BearerTransport = Aeson.String "bearer"
  toJSON CookieTransport = Aeson.String "cookie"

-- | Where a Wasp issuer keeps its credential records.
data CredentialStore
  = -- | A row per credential in the injected @Session@ model.
    PrismaStore
  | -- | The record travels inside a signed token; nothing is stored.
    SignedTokenStore
  | -- | A user-code module exporting a @CredentialStore@.
    CustomStore ExtImport
  deriving (Show, Eq, Data, Generic)

instance FromJSON CredentialStore where
  parseJSON (Aeson.String "prisma") = pure PrismaStore
  parseJSON (Aeson.String "signed-token") = pure SignedTokenStore
  parseJSON value = Aeson.withObject "store" (\o -> CustomStore <$> o .: "module") value

instance ToJSON CredentialStore where
  toJSON PrismaStore = Aeson.String "prisma"
  toJSON SignedTokenStore = Aeson.String "signed-token"
  toJSON (CustomStore extImport) = Aeson.object ["module" .= extImport]

-- | The sibling scheme this scheme signs into, if that is how it hands out
-- credentials.
credentialsScheme :: AuthScheme -> Maybe String
credentialsScheme scheme = case scheme.credentials of
  Just (CredentialsFromScheme schemeName) -> Just schemeName
  _ -> Nothing

-- | The private issuer this scheme is configured with, if that is how it
-- hands out credentials.
inlineCredentials :: AuthScheme -> Maybe (CredentialTransport, CredentialStore, String)
inlineCredentials scheme = case scheme.credentials of
  Just (InlineCredentials transport' store' ttl') -> Just (transport', store', ttl')
  _ -> Nothing

-- | Whether other schemes may sign into this one.
canSignIn :: AuthScheme -> Bool
canSignIn scheme = "sign-in" `elem` scheme.capabilities

-- | Whether any scheme's credential travels as a cookie: a Wasp issuer with
-- the cookie transport, or a handler declaring the "cookie-transport"
-- capability for a cookie of its own.
--
-- Only then does the generated client send requests with
-- @credentials: 'include'@. That mode makes the browser REQUIRE an
-- @Access-Control-Allow-Credentials: true@ response header, which an app that
-- replaced the CORS middleware the documented way (@cors({ origin })@) does
-- not send. Keeping it off for the default bearer setup is what keeps every
-- such app working.
isCookieTransportUsed :: Auth -> Bool
isCookieTransportUsed = any usesCookie . schemes
  where
    usesCookie scheme =
      "cookie-transport" `elem` scheme.capabilities
        || case inlineCredentials scheme of
          Just (CookieTransport, _, _) -> True
          _ -> False

-- | Whether any configured scheme brings a client-side auth handler entry.
isClientAuthHandlerUsed :: Auth -> Bool
isClientAuthHandlerUsed = any (isJust . (.client)) . schemes

schemeNames :: Auth -> [String]
schemeNames = map (.name) . schemes

data AuthSchemeRoutes = AuthSchemeRoutes
  { -- | When true, the scheme's routes are mounted without the JSON body
    -- parser, because the handler reads the raw request body itself.
    rawBody :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | The field is `envVarName` rather than `name` so it does not clash with
-- 'AuthScheme''s `name` under OverloadedRecordDot; the JSON key stays `name`.
data AuthSchemeEnvVar = AuthSchemeEnvVar
  { envVarName :: String,
    optional :: Maybe Bool,
    doc :: Maybe String,
    -- | Development fallback value: applied when the var is unset in dev, so
    -- @wasp start@ works out of the box; production keeps the var required.
    devDefault :: Maybe String
  }
  deriving (Show, Eq, Data, Generic)

authSchemeEnvVarJsonOptions :: Aeson.Options
authSchemeEnvVarJsonOptions =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = \field -> if field == "envVarName" then "name" else field}

instance FromJSON AuthSchemeEnvVar where
  parseJSON = Aeson.genericParseJSON authSchemeEnvVarJsonOptions

instance ToJSON AuthSchemeEnvVar where
  toJSON = Aeson.genericToJSON authSchemeEnvVarJsonOptions

-- | One provider of a scheme. 'providerKind' says what a login through it
-- carries besides the identity: @"oauth"@ (the provider's tokens, which the
-- generated runtime then demands), or nothing.
data AuthSchemeProvider = AuthSchemeProvider
  { providerName :: String,
    providerKind :: Maybe String
  }
  deriving (Show, Eq, Data, Generic)

authSchemeProviderJsonOptions :: Aeson.Options
authSchemeProviderJsonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \field -> case field of
        "providerName" -> "name"
        "providerKind" -> "kind"
        _ -> field
    }

instance FromJSON AuthSchemeProvider where
  parseJSON = Aeson.genericParseJSON authSchemeProviderJsonOptions

instance ToJSON AuthSchemeProvider where
  toJSON = Aeson.genericToJSON authSchemeProviderJsonOptions

providerNames :: AuthScheme -> [String]
providerNames scheme = (.providerName) <$> scheme.providers

onBeforeSignup :: Auth -> Maybe ExtImport
onBeforeSignup auth = hooks auth >>= hooksOnBeforeSignup

onAfterSignup :: Auth -> Maybe ExtImport
onAfterSignup auth = hooks auth >>= hooksOnAfterSignup

onBeforeLogin :: Auth -> Maybe ExtImport
onBeforeLogin auth = hooks auth >>= hooksOnBeforeLogin

onAfterLogin :: Auth -> Maybe ExtImport
onAfterLogin auth = hooks auth >>= hooksOnAfterLogin

onBeforeLink :: Auth -> Maybe ExtImport
onBeforeLink auth = hooks auth >>= hooksOnBeforeLink

onAfterLink :: Auth -> Maybe ExtImport
onAfterLink auth = hooks auth >>= hooksOnAfterLink
