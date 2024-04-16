module Wasp.Generator.WebAppGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (reldirP, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders.OAuth (serverExchangeCodeForTokenUrl)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Common as C
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Generator.WebAppGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<:>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      genCreateAuthRequiredPage spec auth
        <:> pure (C.mkTmplFd [relfile|src/auth/pages/DefaultPageLoader.jsx|])
        <:> genOAuthAuth auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates HOC that handles auth for the given page.
genCreateAuthRequiredPage :: AppSpec -> AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage spec auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/pages/createAuthRequiredPage.jsx|]
      ( object
          [ "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth,
            "pageLoader" .= extImportToImportJson relPathToWebAppSrcDir maybePageLoaderComponent
          ]
      )
  where
    maybePageLoaderComponent = AS.App.pageLoader $ snd $ getApp spec
    relPathToWebAppSrcDir = [reldirP|../../|]

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
