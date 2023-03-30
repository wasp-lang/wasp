module Wasp.Generator.AuthProviders.Email
  ( providerId,
    displayName,
    serverLoginUrl,
    serverSignupUrl,
    serverRequestPasswordResetUrl,
    serverResetPasswordUrl,
    serverVerifyEmailUrl,
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

serverLoginUrl :: EmailAuthProvider -> String
serverLoginUrl provider = "/auth/" ++ providerId provider ++ "/login"

serverSignupUrl :: EmailAuthProvider -> String
serverSignupUrl provider = "/auth/" ++ providerId provider ++ "/signup"

serverRequestPasswordResetUrl :: EmailAuthProvider -> String
serverRequestPasswordResetUrl provider = "/auth/" ++ providerId provider ++ "/request-password-reset"

serverResetPasswordUrl :: EmailAuthProvider -> String
serverResetPasswordUrl provider = "/auth/" ++ providerId provider ++ "/reset-password"

serverVerifyEmailUrl :: EmailAuthProvider -> String
serverVerifyEmailUrl provider = "/auth/" ++ providerId provider ++ "/verify-email"
