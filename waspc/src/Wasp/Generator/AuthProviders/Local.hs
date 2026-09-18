module Wasp.Generator.AuthProviders.Local
  ( providerId,
    displayName,
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
