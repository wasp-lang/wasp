module Wasp.Generator.SdkGenerator.Server.OAuthG
  ( genOAuth,
    depsRequiredByOAuth,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Maybe (fromJust, isJust)
import StrongPath (Dir', File', Path', Rel, reldir, relfile, (</>), parseRelFile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Valid as AS.Valid
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.AuthProviders (discordAuthProvider, getEnabledAuthProvidersJson, gitHubAuthProvider, googleAuthProvider, keycloakAuthProvider, slackAuthProvider)
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
import Wasp.Generator.SdkGenerator.Common
import Wasp.Util ((<++>))
import Wasp.Generator.SdkGenerator.Server.Common

serverOauthDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
serverOauthDirInSdkTemplatesProjectDir = serverTemplatesDirInSdkTemplatesDir </> [reldir|auth/oauth|]

genOAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      sequence
        [ genIndexTs auth,
          genRedirectHelper,
          genFileCopy $ serverOauthDirInSdkTemplatesProjectDir </> [relfile|oneTimeCode.ts|],
          genFileCopy $ serverOauthDirInSdkTemplatesProjectDir </> [relfile|provider.ts|]
        ]
        <++> genOAuthProvider slackAuthProvider (AS.Auth.slack . AS.Auth.methods $ auth)
        <++> genOAuthProvider discordAuthProvider (AS.Auth.discord . AS.Auth.methods $ auth)
        <++> genOAuthProvider googleAuthProvider (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider keycloakAuthProvider (AS.Auth.keycloak . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthProvider (AS.Auth.gitHub . AS.Auth.methods $ auth)
  | otherwise = return []
  where
    genFileCopy = return . makeSdkProjectTmplFd SdkUserCoreProject

genIndexTs :: AS.Auth.Auth -> Generator FileDraft
genIndexTs auth = return $ 
  makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverOauthDirInSdkTemplatesProjectDir </> [relfile|index.ts|]
    tmplData =
      object
        [ "enabledProviders" .= getEnabledAuthProvidersJson auth
        ]

genRedirectHelper :: Generator FileDraft
genRedirectHelper = return $ 
  makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverOauthDirInSdkTemplatesProjectDir </> [relfile|redirect.ts|]
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
genOAuthConfig provider = return $ 
  makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = serverOauthDirInSdkTemplatesProjectDir </> [reldir|providers|] </> providerTsFile
    tmplData =
      object
        [ "providerId" .= OAuth.providerId provider,
          "displayName" .= OAuth.displayName provider
        ]

    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ parseRelFile $ providerId ++ ".ts"

    providerId = OAuth.providerId provider

depsRequiredByOAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByOAuth spec =
  [Npm.Dependency.make ("arctic", "^1.2.1") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True]
  where
    maybeAuth = AS.App.auth $ snd $ AS.Valid.getApp spec
