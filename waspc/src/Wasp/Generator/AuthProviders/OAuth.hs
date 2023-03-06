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
    OAuthAuthInfo (..),
  )
where

import Data.Char (toUpper)
import Data.List (intercalate)
import StrongPath (File', Path', Rel')
import Wasp.AppSpec.App.Dependency (Dependency)

data OAuthAuthInfo = OAuthAuthInfo
  { -- Unique identifier of the auth provider
    _providerId :: String,
    -- Used for pretty printing
    _displayName :: String,
    _requiredScope :: OAuthScope,
    _logoFileName :: Path' Rel' File',
    _passportDependency :: Dependency
  }

type OAuthScope = [String]

providerId :: OAuthAuthInfo -> String
providerId = _providerId

displayName :: OAuthAuthInfo -> String
displayName = _displayName

logoFileName :: OAuthAuthInfo -> Path' Rel' File'
logoFileName = _logoFileName

clientIdEnvVarName :: OAuthAuthInfo -> String
clientIdEnvVarName oai = upperCaseId oai ++ "_CLIENT_ID"

clientSecretEnvVarName :: OAuthAuthInfo -> String
clientSecretEnvVarName oai = upperCaseId oai ++ "_CLIENT_SECRET"

upperCaseId :: OAuthAuthInfo -> String
upperCaseId oai = map toUpper (_providerId oai)

-- Generates the string used in JS e.g. ["profile"] list in Haskell becomes "[\"profile\"]"
-- string which can be outputted in JS code verbatim.
scopeStr :: OAuthAuthInfo -> String
scopeStr oai = "[" ++ intercalate ", " scopeStrs ++ "]"
  where
    scopeStrs = map show (_requiredScope oai)

passportDependency :: OAuthAuthInfo -> Dependency
passportDependency = _passportDependency

frontendLoginUrl :: OAuthAuthInfo -> String
frontendLoginUrl oai = "/auth/login/" ++ _providerId oai

serverLoginUrl :: OAuthAuthInfo -> String
serverLoginUrl oai = "/auth/" ++ _providerId oai ++ "/login"

serverOauthRedirectHandlerUrl :: OAuthAuthInfo -> String
serverOauthRedirectHandlerUrl oai = "/auth/" ++ _providerId oai ++ "/callback"
