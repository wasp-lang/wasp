module Wasp.Generator.AuthProviders.OAuth
  ( frontendLoginUrl,
    serverLoginUrl,
    serverOauthRedirectHandlerUrl,
    slug,
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
  { _slug :: String,
    _displayName :: String,
    _requiredScope :: OAuthScope,
    _logoFileName :: Path' Rel' File',
    _passportDependency :: Dependency
  }

type OAuthScope = [String]

slug :: OAuthAuthInfo -> String
slug = _slug

displayName :: OAuthAuthInfo -> String
displayName = _displayName

logoFileName :: OAuthAuthInfo -> Path' Rel' File'
logoFileName = _logoFileName

clientIdEnvVarName :: OAuthAuthInfo -> String
clientIdEnvVarName oai = upperCaseSlug oai ++ "_CLIENT_ID"

clientSecretEnvVarName :: OAuthAuthInfo -> String
clientSecretEnvVarName oai = upperCaseSlug oai ++ "_CLIENT_SECRET"

upperCaseSlug :: OAuthAuthInfo -> String
upperCaseSlug oai = map toUpper (_slug oai)

scopeStr :: OAuthAuthInfo -> String
scopeStr oai = "[" ++ intercalate ", " scopeStrs ++ "]"
  where
    scopeStrs = map show (_requiredScope oai)

passportDependency :: OAuthAuthInfo -> Dependency
passportDependency = _passportDependency

frontendLoginUrl :: OAuthAuthInfo -> String
frontendLoginUrl oai = "/auth/login/" ++ _slug oai

serverLoginUrl :: OAuthAuthInfo -> String
serverLoginUrl oai = "/auth/" ++ _slug oai ++ "/login"

serverOauthRedirectHandlerUrl :: OAuthAuthInfo -> String
serverOauthRedirectHandlerUrl oai = "/auth/" ++ _slug oai ++ "/callback"
