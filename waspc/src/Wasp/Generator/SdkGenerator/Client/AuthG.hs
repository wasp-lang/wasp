module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genClientAuth,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import StrongPath (Dir', Path', Rel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Auth.Common (waspAuthOptionsJson)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    mkTmplFdWithData,
  )

-- | The uniform client surface (useAuth, logout, resumeSession, the adapter
-- registry) exists for every provider mix. Wasp's own auth UI and actions
-- live in the @wasp.sh/auth lib and are re-exported iff Wasp's own auth is
-- among the providers -- external providers bring their own UI.
genClientAuth :: AppSpec -> Generator [FileDraft]
genClientAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genAuthIndex auth,
          genClientAuthProvidersTs spec auth
        ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | The client halves of the auth providers: instantiates each adapter
-- package's client entry (Wasp's own auth lib included) with the same
-- runtime-window discipline as the server halves, and carries the
-- session-resume and login helpers built on them.
genClientAuthProvidersTs :: AppSpec -> AS.Auth.Auth -> Generator FileDraft
genClientAuthProvidersTs spec auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|providers.ts|])
      tmplData
  where
    tmplData =
      Aeson.object
        [ "anyClientAdapters" Aeson..= (not . null $ clientAdapterProviders),
          "isWaspAuthProviderUsed" Aeson..= AS.Auth.isWaspAuthProviderUsed auth,
          "waspAuthOptionsJson" Aeson..= waspAuthOptionsJson spec auth,
          "clientAdapterProviders" Aeson..= zipWith mkClientAdapterProviderTmplData [0 :: Int ..] clientAdapterProviders
        ]
    clientAdapterProviders =
      [ (extProvider, clientPackage)
      | extProvider <- AS.Auth.externalProviders auth,
        Just clientPackage <- [AS.Auth.clientPackage extProvider]
      ]
    mkClientAdapterProviderTmplData idx (extProvider, clientPackage) =
      Aeson.object
        [ "index" Aeson..= idx,
          "providerId" Aeson..= extProvider.providerId,
          "clientPackage" Aeson..= clientPackage,
          "hasOptions" Aeson..= maybe False (const True) extProvider.optionsJson,
          "optionsJson" Aeson..= extProvider.optionsJson,
          -- The client adapter runtime's env is narrowed to exactly these names.
          "clientEnvVarNamesJs"
            Aeson..= makeJsArrayFromHaskellList ((.name) <$> extProvider.envVars.client)
        ]

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData = case AuthProviders.getEnabledAuthProvidersJson auth of
      Aeson.Object enabledProvidersFlags ->
        Aeson.Object $
          KeyMap.insert
            "isWaspAuthProviderUsed"
            (Aeson.toJSON $ AS.Auth.isWaspAuthProviderUsed auth)
            enabledProvidersFlags
      otherJson -> otherJson

clientAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
clientAuthDirInSdkTemplatesDir = [reldir|client/auth|]
