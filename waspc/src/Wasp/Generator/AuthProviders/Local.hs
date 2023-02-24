module Wasp.Generator.AuthProviders.Local
  ( mkLocalAuthInfo,
    slug,
    serverLoginUrl,
    serverSignupUrl,
    LocalAuthInfo,
  )
where

data LocalAuthInfo = LocalAuthInfo
  { _slug :: String,
    _displayName :: String
  }

mkLocalAuthInfo :: String -> String -> LocalAuthInfo
mkLocalAuthInfo = LocalAuthInfo

slug :: LocalAuthInfo -> String
slug = _slug

serverLoginUrl :: LocalAuthInfo -> String
serverLoginUrl authInfo = "/auth/" ++ _slug authInfo ++ "/login"

serverSignupUrl :: LocalAuthInfo -> String
serverSignupUrl authInfo = "/auth/" ++ _slug authInfo ++ "/signup"
