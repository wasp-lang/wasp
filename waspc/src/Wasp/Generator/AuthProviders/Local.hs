module Wasp.Generator.AuthProviders.Local
  ( providerId,
    displayName,
    serverLoginUrl,
    serverSignupUrl,
    LocalAuthProvider (..),
  )
where

import Wasp.Generator.AuthProviders.Common (ProviderId, fromProviderId)

data LocalAuthProvider = LocalAuthProvider
  { -- Unique identifier of the auth provider
    _providerId :: ProviderId,
    -- Used for pretty printing
    _displayName :: String
  }

providerId :: LocalAuthProvider -> String
providerId = fromProviderId . _providerId

displayName :: LocalAuthProvider -> String
displayName = _displayName

serverLoginUrl :: LocalAuthProvider -> String
serverLoginUrl provider = "/auth/" ++ providerId provider ++ "/login"

serverSignupUrl :: LocalAuthProvider -> String
serverSignupUrl provider = "/auth/" ++ providerId provider ++ "/signup"
