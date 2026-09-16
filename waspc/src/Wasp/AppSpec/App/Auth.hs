{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.AppSpec.App.Auth
  ( Auth (..),
    AuthHooksSpec (..),
    AuthScheme (..),
    AuthSchemeServer (..),
    AuthSchemeRoutes (..),
    AuthSchemeEnvVars (..),
    AuthSchemeEnvVar (..),
    AuthSchemeCredentials (..),
    CredentialTransport (..),
    CredentialStore (..),
    onBeforeSignup,
    onAfterSignup,
    onBeforeLogin,
    onAfterLogin,
    serverPackage,
    serverModule,
    credentialsScheme,
    inlineCredentials,
    canSignIn,
    isClientAuthAdapterUsed,
    userSignupFieldsForAuthScheme,
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

-- | A named, configured instance of an auth handler, as declared in the spec.
--
-- 'capabilities' is deliberately an open set of strings: handlers ship
-- independently of Wasp releases, so a closed enum here would break decoding of
-- every manifest built against a newer handler. Unknown entries are ignored.
data AuthScheme = AuthScheme
  { -- | The scheme name: the key in @auth.schemes@. Sessions record it,
    -- identity namespaces and routes are prefixed with it, and
    -- @authRequired@ lists name it.
    name :: String,
    -- | The handler package the manifest came from (@"\@wasp.sh/auth"@,
    -- @"custom"@). Informational.
    handler :: String,
    -- | Where the handler's implementation comes from.
    server :: AuthSchemeServer,
    -- | Module specifier of a handler package's client entry, if it has one.
    clientPackage :: Maybe String,
    -- | Whether the handler brings its own routes, mounted at @/auth/<name>@.
    routes :: Maybe AuthSchemeRoutes,
    capabilities :: [String],
    envVars :: AuthSchemeEnvVars,
    -- | Runtime facets the handler requests from Wasp ("email-send",
    -- "identity-namespaces"). Validation rejects unknown names: the generator
    -- can only wire facets it knows.
    uses :: [String],
    -- | Every identity namespace this scheme records identities under: the
    -- scheme name itself, plus each declared suffix prefixed with it
    -- (@"wasp:email"@).
    identityNamespaces :: [String],
    -- | How the scheme hands out credentials after a login it verified;
    -- absent for schemes whose own credential authenticates every request.
    credentials :: Maybe AuthSchemeCredentials,
    -- | Populates the user entity when Wasp provisions a local user for a
    -- subject it has not seen before.
    userSignupFields :: Maybe ExtImport,
    -- | Setup function for the handler's underlying library.
    setupFn :: Maybe ExtImport,
    -- | Every other user function the handler calls back into, keyed by the
    -- name the handler expects. Delivered to the handler's server factory
    -- through virtual user modules, like every other user function.
    extensions :: Map String ExtImport,
    -- | The handler's serializable options, JSON-encoded.
    optionsJson :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Exactly one of: a handler package's server entry (module specifier), or a
-- user-code module implementing the handler (the hand-written escape hatch).
newtype AuthSchemeServer = AuthSchemeServer (Either String ExtImport)
  deriving (Show, Eq, Data, Generic)

instance FromJSON AuthSchemeServer where
  parseJSON = Aeson.withObject "server" $ \o -> do
    maybePackage <- o .:? "package"
    maybeModule <- o .:? "module"
    case (maybePackage, maybeModule) of
      (Just packageSpecifier, Nothing) -> pure $ AuthSchemeServer (Left packageSpecifier)
      (Nothing, Just extImport) -> pure $ AuthSchemeServer (Right extImport)
      _ -> fail "server must contain exactly one of 'package' and 'module'"

instance ToJSON AuthSchemeServer where
  toJSON (AuthSchemeServer (Left packageSpecifier)) =
    Aeson.object ["package" .= packageSpecifier]
  toJSON (AuthSchemeServer (Right extImport)) =
    Aeson.object ["module" .= extImport]

serverPackage :: AuthScheme -> Maybe String
serverPackage scheme = case scheme.server of
  AuthSchemeServer (Left packageSpecifier) -> Just packageSpecifier
  AuthSchemeServer (Right _) -> Nothing

serverModule :: AuthScheme -> Maybe ExtImport
serverModule scheme = case scheme.server of
  AuthSchemeServer (Left _) -> Nothing
  AuthSchemeServer (Right extImport) -> Just extImport

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

-- | Whether any configured scheme brings a client-side adapter entry.
isClientAuthAdapterUsed :: Auth -> Bool
isClientAuthAdapterUsed = any (isJust . clientPackage) . schemes

schemeNames :: Auth -> [String]
schemeNames = map (.name) . schemes

data AuthSchemeRoutes = AuthSchemeRoutes
  { -- | When true, the scheme's routes are mounted without the JSON body
    -- parser, because the handler reads the raw request body itself.
    rawBody :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data AuthSchemeEnvVars = AuthSchemeEnvVars
  { server :: [AuthSchemeEnvVar],
    client :: [AuthSchemeEnvVar]
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
userSignupFieldsForAuthScheme :: AuthScheme -> Maybe ExtImport
userSignupFieldsForAuthScheme = (.userSignupFields)
