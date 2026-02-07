module Wasp.Generator.SdkGenerator.Client.AppG
  ( genClientApp,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.AuthProviders.OAuth (clientOAuthCallbackPath)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.SdkGenerator.UserCore.Common (SdkTemplatesUserCoreDir, genFileCopy, mkTmplFdWithData)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Generator.WebSocket as WS
import Wasp.Util ((<++>))

genClientApp :: AppSpec -> Generator [FileDraft]
genClientApp spec =
  sequence
    [ genAppIndex spec,
      genWaspAppComponent spec
    ]
    <++> genAppComponents
    <++> genRouter spec
    <++> genAuthPages spec

genAppIndex :: AppSpec -> Generator FileDraft
genAppIndex spec =
  return $
    mkTmplFdWithData
      (clientAppDirInSdkTemplatesUserCoreDir </> [relfile|index.tsx|])
      (object ["isAuthEnabled" .= isAuthEnabled spec])

genWaspAppComponent :: AppSpec -> Generator FileDraft
genWaspAppComponent spec =
  return $
    mkTmplFdWithData
      (clientAppDirInSdkTemplatesUserCoreDir </> [relfile|components/WaspApp.tsx|])
      (object ["areWebSocketsUsed" .= WS.areWebSocketsUsed spec])

genAppComponents :: Generator [FileDraft]
genAppComponents =
  sequence
    [ genFileCopyInClientApp [relfile|components/Loader.tsx|],
      genFileCopyInClientApp [relfile|components/Loader.module.css|],
      genFileCopyInClientApp [relfile|components/FullPageWrapper.tsx|],
      genFileCopyInClientApp [relfile|components/DefaultRootErrorBoundary.tsx|],
      genFileCopyInClientApp [relfile|components/Message.tsx|]
    ]

genRouter :: AppSpec -> Generator [FileDraft]
genRouter spec =
  sequence
    [ return $
        mkTmplFdWithData
          (clientAppDirInSdkTemplatesUserCoreDir </> [relfile|router/router.tsx|])
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

genAuthPages :: AppSpec -> Generator [FileDraft]
genAuthPages spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence $
        genCreateAuthRequiredPage auth
          : [genOAuthCallbackPage auth | AS.Auth.isExternalAuthEnabled auth]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genCreateAuthRequiredPage :: AS.Auth.Auth -> Generator FileDraft
genCreateAuthRequiredPage auth =
  return $
    mkTmplFdWithData
      (clientAppDirInSdkTemplatesUserCoreDir </> [relfile|pages/createAuthRequiredPage.jsx|])
      (object ["onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth])

genOAuthCallbackPage :: AS.Auth.Auth -> Generator FileDraft
genOAuthCallbackPage auth =
  return $
    mkTmplFdWithData
      (clientAppDirInSdkTemplatesUserCoreDir </> [relfile|pages/OAuthCallback.tsx|])
      (object ["onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth])

genFileCopyInClientApp :: Path' Rel' File' -> Generator FileDraft
genFileCopyInClientApp =
  genFileCopy . (clientAppDirInSdkTemplatesUserCoreDir </>)

clientAppDirInSdkTemplatesUserCoreDir :: Path' (Rel SdkTemplatesUserCoreDir) Dir'
clientAppDirInSdkTemplatesUserCoreDir = [reldir|client/app|]
