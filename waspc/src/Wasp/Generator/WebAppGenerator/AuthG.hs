module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders.OAuth (serverExchangeCodeForTokenUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Common as C
  ( mkTmplFdWithData,
  )
import Wasp.Util ((<:>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      genCreateAuthRequiredPage auth
        <:> genOAuthAuth auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates HOC that handles auth for the given page.
genCreateAuthRequiredPage :: AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/pages/createAuthRequiredPage.jsx|]
      ( object
          [ "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth
          ]
      )

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth = sequence [genOAuthCodeExchange auth | AS.Auth.isExternalAuthEnabled auth]

genOAuthCodeExchange :: AS.Auth.Auth -> Generator FileDraft
genOAuthCodeExchange auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/pages/OAuthCallback.tsx|]
      ( object
          [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
            "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth,
            "serverExchangeCodeForTokenUrl" .= serverExchangeCodeForTokenUrl
          ]
      )
