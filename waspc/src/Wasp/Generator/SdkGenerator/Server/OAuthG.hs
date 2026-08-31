module Wasp.Generator.SdkGenerator.Server.OAuthG
  ( genOAuth,
    depsRequiredByOAuth,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Maybe (fromJust, isJust)
import StrongPath (Dir', File', Path', Rel, Rel', parseRelFile, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Valid as AS.Valid
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.AppDeliveryPlan (AppDeliveryPlan (oauthLoginCompletion), OAuthLoginCompletion (ExchangeSessionHandoffCode))
import Wasp.Generator.AuthProviders (discordAuthProvider, getEnabledAuthProvidersJson, gitHubAuthProvider, googleAuthProvider, keycloakAuthProvider, microsoftAuthProvider, slackAuthProvider)
import Wasp.Generator.AuthProviders.OAuth
  ( OAuthAuthProvider,
    oauthLoginResultPath,
    providerCallbackPath,
    providerLoginPath,
    sessionHandoffExchangePath,
  )
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genOAuth :: AppDeliveryPlan -> AS.Auth.Auth -> Generator [FileDraft]
genOAuth deliveryPlan auth
  | AS.Auth.isExternalAuthEnabled auth =
      sequence
        ( [ genIndexTs deliveryPlan auth,
            genRedirectHelper deliveryPlan,
            genOAuthLoginCompletion deliveryPlan,
            genFileCopyInServerOAuth [relfile|provider.ts|]
          ]
            ++ [genFileCopyInServerOAuth [relfile|sessionHandoff.ts|] | usesSessionHandoff]
        )
        <++> genOAuthProvider slackAuthProvider (AS.Auth.slack . AS.Auth.methods $ auth)
        <++> genOAuthProvider discordAuthProvider (AS.Auth.discord . AS.Auth.methods $ auth)
        <++> genOAuthProvider googleAuthProvider (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider keycloakAuthProvider (AS.Auth.keycloak . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthProvider (AS.Auth.gitHub . AS.Auth.methods $ auth)
        <++> genOAuthProvider microsoftAuthProvider (AS.Auth.microsoft . AS.Auth.methods $ auth)
  | otherwise = return []
  where
    usesSessionHandoff = oauthLoginCompletion deliveryPlan == ExchangeSessionHandoffCode

genIndexTs :: AppDeliveryPlan -> AS.Auth.Auth -> Generator FileDraft
genIndexTs deliveryPlan auth =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData =
      object
        [ "enabledProviders" .= getEnabledAuthProvidersJson auth,
          "usesSessionHandoff" .= (oauthLoginCompletion deliveryPlan == ExchangeSessionHandoffCode)
        ]

genRedirectHelper :: AppDeliveryPlan -> Generator FileDraft
genRedirectHelper deliveryPlan =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [relfile|redirect.ts|])
      tmplData
  where
    tmplData =
      object
        [ "providerCallbackPath" .= providerCallbackPath,
          "oauthLoginResultPath" .= oauthLoginResultPath,
          "providerLoginPath" .= providerLoginPath,
          "sessionHandoffExchangePath" .= sessionHandoffExchangePath,
          "usesSessionHandoff" .= (oauthLoginCompletion deliveryPlan == ExchangeSessionHandoffCode)
        ]

genOAuthLoginCompletion :: AppDeliveryPlan -> Generator FileDraft
genOAuthLoginCompletion deliveryPlan =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [relfile|completeOAuthLogin.ts|])
      (object ["usesSessionHandoff" .= (oauthLoginCompletion deliveryPlan == ExchangeSessionHandoffCode)])

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
genOAuthConfig provider =
  return $
    mkTmplFdWithData
      (serverOAuthDirInSdkTemplatesDir </> [reldir|providers|] </> providerTsFile)
      tmplData
  where
    tmplData =
      object
        [ "providerId" .= OAuth.providerId provider,
          "displayName" .= OAuth.displayName provider
        ]

    providerTsFile = fromJust $ parseRelFile $ OAuth.providerId provider ++ ".ts"

depsRequiredByOAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByOAuth spec =
  [Npm.Dependency.make ("arctic", "^1.2.1") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True]
  where
    maybeAuth = AS.App.auth $ snd $ AS.Valid.getApp spec

serverOAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverOAuthDirInSdkTemplatesDir = [reldir|server/auth/oauth|]

genFileCopyInServerOAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInServerOAuth = genFileCopy . (serverOAuthDirInSdkTemplatesDir </>)
