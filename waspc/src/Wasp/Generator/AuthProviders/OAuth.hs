module Wasp.Generator.AuthProviders.OAuth
  ( frontendLoginUrl,
    serverLoginUrl,
    serverOauthRedirectHandlerUrl,
    providerId,
    displayName,
    logoFileName,
    passportDependency,
    scopeStr,
    clientIdEnvVarName,
    clientSecretEnvVarName,
    OAuthAuthProvider (..),
  )
where

import Data.Char (toUpper)
import Data.List (intercalate)
import StrongPath (File', Path', Rel')
import Wasp.AppSpec.App.Dependency (Dependency)
import Wasp.Generator.AuthProviders.Common (ProviderId, fromProviderId)

data OAuthAuthProvider = OAuthAuthProvider
  { -- Unique identifier of the auth provider
    _providerId :: ProviderId,
    -- Used for pretty printing
    _displayName :: String,
    _requiredScope :: OAuthScope,
    _logoFileName :: Path' Rel' File',
    _passportDependency :: Dependency
  }

type OAuthScope = [String]

providerId :: OAuthAuthProvider -> String
providerId = fromProviderId . _providerId

displayName :: OAuthAuthProvider -> String
displayName = _displayName

logoFileName :: OAuthAuthProvider -> Path' Rel' File'
logoFileName = _logoFileName

clientIdEnvVarName :: OAuthAuthProvider -> String
clientIdEnvVarName oai = upperCaseId oai ++ "_CLIENT_ID"

clientSecretEnvVarName :: OAuthAuthProvider -> String
clientSecretEnvVarName oai = upperCaseId oai ++ "_CLIENT_SECRET"

upperCaseId :: OAuthAuthProvider -> String
upperCaseId oai = map toUpper (providerId oai)

-- Generates the string used in JS e.g. ["profile"] list in Haskell becomes "[\"profile\"]"
-- string which can be outputted in JS code verbatim.
scopeStr :: OAuthAuthProvider -> String
scopeStr oai = "[" ++ intercalate ", " scopeStrs ++ "]"
  where
    scopeStrs = map show (_requiredScope oai)

passportDependency :: OAuthAuthProvider -> Dependency
passportDependency = _passportDependency

frontendLoginUrl :: OAuthAuthProvider -> String
frontendLoginUrl oai = "/auth/login/" ++ providerId oai

serverLoginUrl :: OAuthAuthProvider -> String
serverLoginUrl oai = "/auth/" ++ providerId oai ++ "/login"

serverOauthRedirectHandlerUrl :: OAuthAuthProvider -> String
serverOauthRedirectHandlerUrl oai = "/auth/" ++ providerId oai ++ "/callback"
