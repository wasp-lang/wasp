module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genClientAuth,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genClientAuth :: AppSpec -> Generator [FileDraft]
genClientAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth
      -- Under an external provider `wasp/client/auth` keeps only the uniform
      -- surface (useAuth, logout); the provider's own UI comes from its
      -- adapter package, not from Wasp's generated forms.
      | AS.Auth.isExternalAuthProviderUsed auth ->
          sequence $
            [genAuthIndex auth]
              ++ [genClientAuthProviderTs auth | AS.Auth.isClientAuthAdapterUsed auth]
      | otherwise ->
          sequence
            [ genAuthIndex auth,
              genAuthUi auth
            ]
            <++> genAuthEmail auth
            <++> genAuthUsername auth
            <++> genAuthSlack auth
            <++> genAuthDiscord auth
            <++> genAuthGoogle auth
            <++> genAuthKeycloak auth
            <++> genAuthGitHub auth
            <++> genAuthMicrosoft auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | The client half of the auth provider: instantiates the adapter package's
-- client entry with the same runtime-window discipline as the server half.
genClientAuthProviderTs :: AS.Auth.Auth -> Generator FileDraft
genClientAuthProviderTs auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|provider.ts|])
      tmplData
  where
    tmplData =
      Aeson.object
        [ "clientPackage" Aeson..= clientPackage,
          "hasOptions" Aeson..= maybe False (const True) maybeOptionsJson,
          "optionsJson" Aeson..= maybeOptionsJson
        ]
    clientPackage = AS.Auth.externalProvider auth >>= AS.Auth.clientPackage
    maybeOptionsJson = AS.Auth.externalProvider auth >>= AS.Auth.optionsJson

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
            "isExternalAuthProviderUsed"
            (Aeson.toJSON $ AS.Auth.isExternalAuthProviderUsed auth)
            enabledProvidersFlags
      otherJson -> otherJson

genAuthUi :: AS.Auth.Auth -> Generator FileDraft
genAuthUi auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|ui.ts|])
      tmplData
  where
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if AS.Auth.isEmailAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|email.ts|]]
    else return []

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if AS.Auth.isUsernameAndPasswordAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|username.ts|]]
    else return []

genAuthSlack :: AS.Auth.Auth -> Generator [FileDraft]
genAuthSlack auth =
  if AS.Auth.isSlackAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|slack.ts|]]
    else return []

genAuthDiscord :: AS.Auth.Auth -> Generator [FileDraft]
genAuthDiscord auth =
  if AS.Auth.isDiscordAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|discord.ts|]]
    else return []

genAuthGoogle :: AS.Auth.Auth -> Generator [FileDraft]
genAuthGoogle auth =
  if AS.Auth.isGoogleAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|google.ts|]]
    else return []

genAuthKeycloak :: AS.Auth.Auth -> Generator [FileDraft]
genAuthKeycloak auth =
  if AS.Auth.isKeycloakAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|keycloak.ts|]]
    else return []

genAuthGitHub :: AS.Auth.Auth -> Generator [FileDraft]
genAuthGitHub auth =
  if AS.Auth.isGitHubAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|github.ts|]]
    else return []

genAuthMicrosoft :: AS.Auth.Auth -> Generator [FileDraft]
genAuthMicrosoft auth =
  if AS.Auth.isMicrosoftAuthEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|microsoft.ts|]]
    else return []

clientAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
clientAuthDirInSdkTemplatesDir = [reldir|client/auth|]

genFileCopyInClientAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInClientAuth =
  genFileCopy . (clientAuthDirInSdkTemplatesDir </>)
