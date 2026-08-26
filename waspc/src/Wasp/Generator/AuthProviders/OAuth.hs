module Wasp.Generator.AuthProviders.OAuth
  ( oauthLoginResultPath,
    providerLoginPath,
    providerCallbackPath,
    providerLoginRoute,
    sessionHandoffExchangePath,
    providerId,
    displayName,
    scopeStr,
    OAuthAuthProvider (..),
  )
where

import Wasp.Generator.AuthProviders.Common (ProviderId, fromProviderId)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)

data OAuthAuthProvider = OAuthAuthProvider
  { -- Unique identifier of the auth provider
    _providerId :: ProviderId,
    -- Used for pretty printing
    _displayName :: String,
    _requiredScope :: OAuthScope
  }

type OAuthScope = [String]

providerId :: OAuthAuthProvider -> String
providerId = fromProviderId . _providerId

displayName :: OAuthAuthProvider -> String
displayName = _displayName

-- Generates the string used in JS e.g. ["profile"] list in Haskell becomes "[\"profile\"]"
-- string which can be outputted in JS code verbatim.
scopeStr :: OAuthAuthProvider -> String
scopeStr oai = makeJsArrayFromHaskellList $ _requiredScope oai

oauthLoginResultPath :: String
oauthLoginResultPath = "/oauth/callback"

providerLoginPath :: String
providerLoginPath = "login"

providerLoginRoute :: OAuthAuthProvider -> String
providerLoginRoute provider = "/auth/" ++ providerId provider ++ "/" ++ providerLoginPath

providerCallbackPath :: String
providerCallbackPath = "callback"

sessionHandoffExchangePath :: String
sessionHandoffExchangePath = "exchange-session-handoff"
