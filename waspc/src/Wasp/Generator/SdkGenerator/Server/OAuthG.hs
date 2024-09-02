module Wasp.Generator.SdkGenerator.Server.OAuthG
  ( genOAuth,
    depsRequiredByOAuth,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Maybe (fromJust, isJust)
import StrongPath (Dir', File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.AuthProviders (discordAuthProvider, getEnabledAuthProvidersJson, gitHubAuthProvider, googleAuthProvider, keycloakAuthProvider)
import Wasp.Generator.AuthProviders.OAuth
  ( OAuthAuthProvider,
    clientOAuthCallbackPath,
    serverExchangeCodeForTokenHandlerPath,
    serverOAuthCallbackHandlerPath,
    serverOAuthLoginHandlerPath,
  )
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util ((<++>))

genOAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      sequence
        [ genIndexTs auth,
          genRedirectHelper,
          genFileCopy $ oauthDirInSdkTemplatesDir </> [relfile|env.ts|],
          genFileCopy $ oauthDirInSdkTemplatesDir </> [relfile|oneTimeCode.ts|],
          genFileCopy $ oauthDirInSdkTemplatesDir </> [relfile|provider.ts|]
        ]
        <++> genOAuthProvider discordAuthProvider (AS.Auth.discord . AS.Auth.methods $ auth)
        <++> genOAuthProvider googleAuthProvider (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider keycloakAuthProvider (AS.Auth.keycloak . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthProvider (AS.Auth.gitHub . AS.Auth.methods $ auth)
  | otherwise = return []
  where
    genFileCopy = return . C.mkTmplFd

genIndexTs :: AS.Auth.Auth -> Generator FileDraft
genIndexTs auth = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = oauthDirInSdkTemplatesDir </> [relfile|index.ts|]
    tmplData =
      object
        [ "enabledProviders" .= getEnabledAuthProvidersJson auth
        ]

genRedirectHelper :: Generator FileDraft
genRedirectHelper = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = oauthDirInSdkTemplatesDir </> [relfile|redirect.ts|]
    tmplData =
      object
        [ "serverOAuthCallbackHandlerPath" .= serverOAuthCallbackHandlerPath,
          "clientOAuthCallbackPath" .= clientOAuthCallbackPath,
          "serverOAuthLoginHandlerPath" .= serverOAuthLoginHandlerPath,
          "serverExchangeCodeForTokenHandlerPath" .= serverExchangeCodeForTokenHandlerPath
        ]

genOAuthProvider ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator [FileDraft]
genOAuthProvider provider maybeUserConfig
  | isJust maybeUserConfig = sequence [genOAuthConfig provider]
  | otherwise = return []

genOAuthConfig ::
  OAuthAuthProvider ->
  Generator FileDraft
genOAuthConfig provider = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = oauthDirInSdkTemplatesDir </> [reldir|providers|] </> providerTsFile
    tmplData =
      object
        [ "providerId" .= OAuth.providerId provider,
          "displayName" .= OAuth.displayName provider
        ]

    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ providerId ++ ".ts"

    providerId = OAuth.providerId provider

depsRequiredByOAuth :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByOAuth spec =
  [AS.Dependency.make ("arctic", "^1.2.1") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True]
  where
    maybeAuth = AS.App.auth $ snd $ AS.Valid.getApp spec

oauthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
oauthDirInSdkTemplatesDir = [reldir|server/auth/oauth|]
