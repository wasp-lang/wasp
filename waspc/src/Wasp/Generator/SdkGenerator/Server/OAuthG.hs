module Wasp.Generator.SdkGenerator.Server.OAuthG
  ( genOAuth,
    depsRequiredByOAuth,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Maybe (fromJust)
import StrongPath (Dir', File', Path', Rel, Rel', parseRelFile, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Valid as AS.Valid
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    clientOAuthCallbackPath,
    enabledAuthMethodsJson,
    enabledOAuthProviders,
    isOAuthEnabled,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genOAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuth auth
  | isOAuthEnabled auth =
      sequence
        [ genIndexTs auth,
          genRedirectHelper,
          genFileCopyInServerOAuth [relfile|oneTimeCode.ts|],
          genFileCopyInServerOAuth [relfile|provider.ts|]
        ]
        <++> concatMapM genSingleProvider (enabledOAuthProviders auth)
  | otherwise = return []

genIndexTs :: AS.Auth.Auth -> Generator FileDraft
genIndexTs auth =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "enabledProviders" .= enabledAuthMethodsJson auth
        ]

genRedirectHelper :: Generator FileDraft
genRedirectHelper =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [relfile|redirect.ts|])
      tmplData
  where
    tmplData =
      object
        [ "serverOAuthCallbackHandlerPath" .= ("callback" :: String),
          "clientOAuthCallbackPath" .= clientOAuthCallbackPath,
          "serverOAuthLoginHandlerPath" .= ("login" :: String),
          "serverExchangeCodeForTokenHandlerPath" .= ("exchange-code" :: String)
        ]

genSingleProvider :: (OAuthProviderSpec, AS.Auth.ExternalAuthConfig) -> Generator [FileDraft]
genSingleProvider (spec, _config) = sequence [genOAuthConfig spec]

genOAuthConfig :: OAuthProviderSpec -> Generator FileDraft
genOAuthConfig spec =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [reldir|providers|] </> providerTsFile)
      tmplData
  where
    tmplData =
      object
        [ "providerId" .= slug spec,
          "displayName" .= displayName spec
        ]

    providerTsFile = fromJust $ parseRelFile $ slug spec ++ ".ts"

depsRequiredByOAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByOAuth spec =
  [Npm.Dependency.make ("arctic", "^1.2.1") | (isOAuthEnabled <$> maybeAuth) == Just True]
  where
    maybeAuth = AS.App.auth $ snd $ AS.Valid.getApp spec

serverOAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverOAuthDirInSdkTemplatesDir = [reldir|server/auth/oauth|]

genFileCopyInServerOAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInServerOAuth = genFileCopy . (serverOAuthDirInSdkTemplatesDir </>)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs
