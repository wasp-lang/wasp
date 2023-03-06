module Wasp.Generator.AuthProviders.Local
  ( providerId,
    displayName,
    serverLoginUrl,
    serverSignupUrl,
    LocalAuthInfo (..),
  )
where

data LocalAuthInfo = LocalAuthInfo
  { -- Unique identifier of the auth provider
    _providerId :: String,
    -- Used for pretty printing
    _displayName :: String
  }

providerId :: LocalAuthInfo -> String
providerId = _providerId

displayName :: LocalAuthInfo -> String
displayName = _displayName

serverLoginUrl :: LocalAuthInfo -> String
serverLoginUrl authInfo = "/auth/" ++ _providerId authInfo ++ "/login"

serverSignupUrl :: LocalAuthInfo -> String
serverSignupUrl authInfo = "/auth/" ++ _providerId authInfo ++ "/signup"
