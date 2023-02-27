module Wasp.Generator.AuthProviders.Local
  ( slug,
    displayName,
    serverLoginUrl,
    serverSignupUrl,
    LocalAuthInfo (..),
  )
where

data LocalAuthInfo = LocalAuthInfo
  { _slug :: String,
    _displayName :: String
  }

slug :: LocalAuthInfo -> String
slug = _slug

displayName :: LocalAuthInfo -> String
displayName = _displayName

serverLoginUrl :: LocalAuthInfo -> String
serverLoginUrl authInfo = "/auth/" ++ _slug authInfo ++ "/login"

serverSignupUrl :: LocalAuthInfo -> String
serverSignupUrl authInfo = "/auth/" ++ _slug authInfo ++ "/signup"
