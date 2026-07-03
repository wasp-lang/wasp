module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genClientAuth,
  )
where

import Data.Aeson (object, (.=))
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
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))

genClientAuth :: AppSpec -> Generator [FileDraft]
genClientAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genAuthIndex auth,
          genAuthUi auth,
          genAuthRedirect auth
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

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|index.ts|])
      tmplData
  where
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthUi :: AS.Auth.Auth -> Generator FileDraft
genAuthUi auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|ui.ts|])
      tmplData
  where
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthRedirect :: AS.Auth.Auth -> Generator FileDraft
genAuthRedirect auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|redirect.ts|])
      tmplData
  where
    tmplData =
      object
        ["onAuthSucceededRedirectFn" .= extImportToImportJson (AS.Auth.onAuthSucceededRedirect auth)]

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
