module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genClientAuth,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir', Path', Rel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )

-- | The uniform client surface (useAuth, logout, resumeSession, the adapter
-- registry) exists for every provider mix. Provider UI (forms, sign-in
-- buttons) comes from each provider package's own client entry.
genClientAuth :: AppSpec -> Generator [FileDraft]
genClientAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genFileCopy (clientAuthDirInSdkTemplatesDir </> [relfile|index.ts|]),
          genClientAuthProvidersTs auth
        ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | The client halves of the auth providers: instantiates each adapter
-- package's client entry with the same runtime-window discipline as the
-- server halves, and carries the session-resume and login helpers built on
-- them.
genClientAuthProvidersTs :: AS.Auth.Auth -> Generator FileDraft
genClientAuthProvidersTs auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|providers.ts|])
      tmplData
  where
    tmplData =
      Aeson.object
        [ "anyClientAdapters" Aeson..= (not . null $ clientAdapterProviders),
          "clientAdapterProviders" Aeson..= zipWith mkClientAdapterProviderTmplData [0 :: Int ..] clientAdapterProviders
        ]
    clientAdapterProviders =
      [ (provider, clientPackage)
      | provider <- AS.Auth.providers auth,
        Just clientPackage <- [AS.Auth.clientPackage provider]
      ]
    mkClientAdapterProviderTmplData idx (provider, clientPackage) =
      Aeson.object
        [ "index" Aeson..= idx,
          "providerId" Aeson..= provider.providerId,
          "clientPackage" Aeson..= clientPackage,
          "hasOptions" Aeson..= maybe False (const True) provider.optionsJson,
          "optionsJson" Aeson..= provider.optionsJson,
          -- The client adapter runtime's env is narrowed to exactly these names.
          "clientEnvVarNamesJs"
            Aeson..= makeJsArrayFromHaskellList ((.name) <$> provider.envVars.client)
        ]

clientAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
clientAuthDirInSdkTemplatesDir = [reldir|client/auth|]
