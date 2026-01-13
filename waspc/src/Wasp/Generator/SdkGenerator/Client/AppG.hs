module Wasp.Generator.SdkGenerator.Client.AppG
  ( genClientApp,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders.OAuth (clientOAuthCallbackPath)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Generator.WebSocket as WS
import Wasp.Util ((<++>))

genClientApp :: AppSpec -> Generator [FileDraft]
genClientApp spec =
  sequence
    [ genAppIndex,
      genWaspAppComponent spec
    ]
    <++> genAppComponents
    <++> genRouter spec
    <++> genAuthPages spec

-- | Generates the main app index file that exports getWaspApp
genAppIndex :: Generator FileDraft
genAppIndex = return $ genFileCopy [relfile|client/app/index.tsx|]

-- | Generates the WaspApp component with QueryClientProvider and optional WebSocket
genWaspAppComponent :: AppSpec -> Generator FileDraft
genWaspAppComponent spec =
  return $
    C.mkTmplFdWithData
      [relfile|client/app/components/WaspApp.tsx|]
      (object ["areWebSocketsUsed" .= WS.areWebSocketsUsed spec])

-- | Generates static UI components used by the app
genAppComponents :: Generator [FileDraft]
genAppComponents =
  return $
    map
      genFileCopy
      [ [relfile|client/app/components/Loader.tsx|],
        [relfile|client/app/components/Loader.module.css|],
        [relfile|client/app/components/FullPageWrapper.tsx|],
        [relfile|client/app/components/DefaultRootErrorBoundary.tsx|],
        [relfile|client/app/components/Message.tsx|]
      ]

-- | Generates the router with OAuth callback route if external auth is enabled
genRouter :: AppSpec -> Generator [FileDraft]
genRouter spec =
  sequence
    [ return $
        C.mkTmplFdWithData
          [relfile|client/app/router/router.tsx|]
          ( object
              [ "isExternalAuthEnabled" .= isExternalAuthEnabled,
                "oAuthCallbackPath" .= clientOAuthCallbackPath,
                "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec)
              ]
          )
    ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    isExternalAuthEnabled = maybe False AS.Auth.isExternalAuthEnabled maybeAuth

-- | Generates auth-related pages (only if auth is enabled)
genAuthPages :: AppSpec -> Generator [FileDraft]
genAuthPages spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence $
        [genCreateAuthRequiredPage auth]
          ++ [genOAuthCallbackPage auth | AS.Auth.isExternalAuthEnabled auth]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates the HOC for auth-protected pages
genCreateAuthRequiredPage :: AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/app/pages/createAuthRequiredPage.jsx|]
      (object ["onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth])

-- | Generates the OAuth callback page
genOAuthCallbackPage :: AS.Auth.Auth -> Generator FileDraft
genOAuthCallbackPage auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/app/pages/OAuthCallback.tsx|]
      (object ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth])

genFileCopy :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
genFileCopy = C.mkTmplFd
