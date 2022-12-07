module Wasp.Generator.WebAppGenerator.ExternalAuthG
  ( googleAuthInfo,
    gitHubAuthInfo,
    ExternalAuthInfo (..),
  )
where

data ExternalAuthInfo = ExternalAuthInfo
  { _frontendLoginUrl :: String,
    _serverLoginUrl :: String,
    _serverOauthRedirectHandlerUrl :: String,
    _logoUrl :: String,
    _displayName :: String
  }

googleAuthInfo :: ExternalAuthInfo
googleAuthInfo =
  ExternalAuthInfo
    { _frontendLoginUrl = "/auth/login/google",
      _serverLoginUrl = "/auth/external/google/login",
      _serverOauthRedirectHandlerUrl = "/auth/external/google/validateCodeForLogin",
      _logoUrl = "/images/google-logo-icon.png",
      _displayName = "Google"
    }

gitHubAuthInfo :: ExternalAuthInfo
gitHubAuthInfo =
  ExternalAuthInfo
    { _frontendLoginUrl = "/auth/login/github",
      _serverLoginUrl = "/auth/external/github/login",
      _serverOauthRedirectHandlerUrl = "/auth/external/github/validateCodeForLogin",
      _logoUrl = "/images/github-logo-icon.png",
      _displayName = "GitHub"
    }
