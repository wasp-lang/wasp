module Wasp.AppSpec.App.Auth.IsEnabled where

import Language.Haskell.TH
import Wasp.AppSpec.App.Auth (Auth (..), AuthMethods (..), generateIsAuthMethodEnabled)
import Wasp.AppSpec.App.Auth.AuthMethods (AuthMethod(..), isAuthMethodExternal)

$(generateIsAuthMethodEnabled)

isExternalAuthEnabled :: Auth -> Bool
isExternalAuthEnabled auth = any check [minBound .. maxBound :: AuthMethod]
  where
    check method = isAuthMethodExternal method && isAuthMethodEnabled method auth