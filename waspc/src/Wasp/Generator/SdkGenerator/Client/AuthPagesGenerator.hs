module Wasp.Generator.SdkGenerator.Client.AuthPagesGenerator
  ( genClientAuthPages,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Auth.Common as Auth
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util ((<:>))

genClientAuthPages :: AppSpec -> Generator [FileDraft]
genClientAuthPages spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      genCreateAuthRequiredPage auth
        <:> genOAuthCallback auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genCreateAuthRequiredPage :: AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/auth/pages/createAuthRequiredPage.jsx|]
      ( object
          [ "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth
          ]
      )

genOAuthCallback :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthCallback auth = sequence [genOAuthCallbackPage auth | AS.Auth.isExternalAuthEnabled auth]

genOAuthCallbackPage :: AS.Auth.Auth -> Generator FileDraft
genOAuthCallbackPage auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/auth/pages/OAuthCallback.tsx|]
      ( object
          [ "onAuthSucceededRedirectTo" .= Auth.getOnAuthSucceededRedirectToOrDefault auth,
            "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth
          ]
      )
