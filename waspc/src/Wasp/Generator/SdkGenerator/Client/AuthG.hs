module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genClientAuth,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
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
import Wasp.Generator.SdkGenerator.JsImport (extImportToAliasedImportJson)

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
          genFileCopy (clientAuthDirInSdkTemplatesDir </> [relfile|types.ts|]),
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
      (clientAuthDirInSdkTemplatesDir </> [relfile|schemes.ts|])
      tmplData
  where
    tmplData =
      Aeson.object
        [ "anyClientAdapters" Aeson..= (not . null $ clientAdapterProviders),
          "defaultScheme" Aeson..= AS.Auth.defaultScheme auth,
          "clientAdapterProviders" Aeson..= zipWith mkClientAdapterProviderTmplData [0 :: Int ..] clientAdapterProviders
        ]
    -- A scheme's client half is a package entry or a factory in the app's
    -- own code; both are instantiated the same way.
    clientAdapterProviders =
      [ scheme
      | scheme <- AS.Auth.schemes auth,
        isJust scheme.client
      ]
    mkClientAdapterProviderTmplData idx provider =
      Aeson.object
        [ "index" Aeson..= idx,
          "schemeName" Aeson..= provider.name,
          "isPackage" Aeson..= isJust (AS.Auth.clientPackage provider),
          "clientPackage" Aeson..= AS.Auth.clientPackage provider,
          "clientModule"
            Aeson..= extImportToAliasedImportJson ("authClientModule_" ++ show idx) (AS.Auth.clientModule provider),
          "hasOptions" Aeson..= maybe False (const True) provider.optionsJson,
          "optionsJson" Aeson..= provider.optionsJson,
          -- The client adapter runtime's env is narrowed to exactly these names.
          "clientEnvVarNamesJs"
            Aeson..= makeJsArrayFromHaskellList ((.envVarName) <$> provider.envVars.client)
        ]

clientAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
clientAuthDirInSdkTemplatesDir = [reldir|client/auth|]
