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
clientIdEnvVarName eai = upperCaseSlug eai ++ "_CLIENT_ID"

clientSecretEnvVarName :: OAuthAuthInfo -> String
clientSecretEnvVarName eai = upperCaseSlug eai ++ "_CLIENT_SECRET"

upperCaseSlug :: OAuthAuthInfo -> String
upperCaseSlug eai = map toUpper (_slug eai)

scopeStr :: OAuthAuthInfo -> String
scopeStr eai = "[" ++ intercalate ", " scopeStrs ++ "]"
  where
    scopeStrs = map show (_requiredScope eai)

passportDependency :: OAuthAuthInfo -> Dependency
passportDependency = _passportDependency

frontendLoginUrl :: OAuthAuthInfo -> String
frontendLoginUrl eai = "/auth/login/" ++ _slug eai

serverLoginUrl :: OAuthAuthInfo -> String
serverLoginUrl eai = "/auth/" ++ _slug eai ++ "/login"

serverOauthRedirectHandlerUrl :: OAuthAuthInfo -> String
serverOauthRedirectHandlerUrl eai = "/auth/" ++ _slug eai ++ "/callback"
