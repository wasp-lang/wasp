module Wasp.Generator.AuthProviders.Email
  ( providerId,
    displayName,
    EmailAuthProvider (..),
  )
where

import Wasp.Generator.AuthProviders.Common (ProviderId, fromProviderId)

data EmailAuthProvider = EmailAuthProvider
  { -- Unique identifier of the auth provider
    _providerId :: ProviderId,
    -- Used for pretty printing
    _displayName :: String
  }

providerId :: EmailAuthProvider -> String
providerId = fromProviderId . _providerId

displayName :: EmailAuthProvider -> String
displayName = _displayName
