module Wasp.Generator.WebAppGenerator.ExternalAuthG
  ( googleAuthInfo,
    gitHubAuthInfo,
    frontendLoginUrl,
    serverLoginUrl,
    serverOauthRedirectHandlerUrl,
    ExternalAuthInfo (..),
  )
where

import StrongPath (File', Path', Rel', relfile)

data ExternalAuthInfo = ExternalAuthInfo
  { _logoFileName :: Path' Rel' File',
    _displayName :: String,
    _slug :: String
  }

googleAuthInfo :: ExternalAuthInfo
googleAuthInfo =
  ExternalAuthInfo
    { _logoFileName = [relfile|google-logo-icon.png|],
      _displayName = "Google",
      _slug = "google"
    }

gitHubAuthInfo :: ExternalAuthInfo
gitHubAuthInfo =
  ExternalAuthInfo
    { _logoFileName = [relfile|github-logo-icon.png|],
      _displayName = "GitHub",
      _slug = "github"
    }

frontendLoginUrl :: ExternalAuthInfo -> String
frontendLoginUrl eai = "/auth/login/" ++ _slug eai

serverLoginUrl :: ExternalAuthInfo -> String
serverLoginUrl eai = "/auth/external/" ++ _slug eai ++ "/login"

serverOauthRedirectHandlerUrl :: ExternalAuthInfo -> String
serverOauthRedirectHandlerUrl eai = "/auth/external/" ++ _slug eai ++ "/validateCodeForLogin"
