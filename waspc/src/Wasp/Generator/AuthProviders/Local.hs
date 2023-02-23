module Wasp.Generator.AuthProviders.Local where

data LocalAuthInfo = LocalAuthInfo
  { _slug :: String,
    _displayName :: String
  }

mkLocalAuthInfo :: String -> String -> LocalAuthInfo
mkLocalAuthInfo = LocalAuthInfo

localAuthInfo :: LocalAuthInfo
localAuthInfo =
  LocalAuthInfo
    { _displayName = "Username and password",
      _slug = "local"
    }

serverLoginUrl :: LocalAuthInfo -> String
serverLoginUrl eai = "/auth/" ++ _slug eai ++ "/login"

serverSignupUrl :: LocalAuthInfo -> String
serverSignupUrl eai = "/auth/" ++ _slug eai ++ "/signup"
