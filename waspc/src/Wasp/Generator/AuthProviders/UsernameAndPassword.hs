module Wasp.Generator.AuthProviders.UsernameAndPassword
  ( providerId,
    displayName,
    serverLoginUrl,
    serverSignupUrl,
    UsernameAndPasswordAuthProvider (..),
  )
where

import Wasp.Generator.AuthProviders.Common (ProviderId, fromProviderId)

data UsernameAndPasswordAuthProvider = UsernameAndPasswordAuthProvider
  { -- Unique identifier of the auth provider
    _providerId :: ProviderId,
    -- Used for pretty printing
    _displayName :: String
  }

providerId :: UsernameAndPasswordAuthProvider -> String
providerId = fromProviderId . _providerId

displayName :: UsernameAndPasswordAuthProvider -> String
displayName = _displayName

serverLoginUrl :: UsernameAndPasswordAuthProvider -> String
serverLoginUrl provider = "/auth/" ++ providerId provider ++ "/login"

serverSignupUrl :: UsernameAndPasswordAuthProvider -> String
serverSignupUrl provider = "/auth/" ++ providerId provider ++ "/signup"
