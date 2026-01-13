module Wasp.Generator.SdkGenerator.UserCore.Client.AuthG
  ( genNewClientAuth,
  )
where

import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( UserCoreTemplatesDir,
    mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Util ((<++>))

genNewClientAuth :: AppSpec -> Generator [FileDraft]
genNewClientAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
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
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = clientAuthDirInUserCoreTemplatesDir </> [relfile|index.ts|]
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthUi :: AS.Auth.Auth -> Generator FileDraft
genAuthUi auth =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = clientAuthDirInUserCoreTemplatesDir </> [relfile|ui.ts|]
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if AS.Auth.isEmailAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|email.ts|]]
    else return []

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if AS.Auth.isUsernameAndPasswordAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|username.ts|]]
    else return []

genAuthSlack :: AS.Auth.Auth -> Generator [FileDraft]
genAuthSlack auth =
  if AS.Auth.isSlackAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|slack.ts|]]
    else return []

genAuthDiscord :: AS.Auth.Auth -> Generator [FileDraft]
genAuthDiscord auth =
  if AS.Auth.isDiscordAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|discord.ts|]]
    else return []

genAuthGoogle :: AS.Auth.Auth -> Generator [FileDraft]
genAuthGoogle auth =
  if AS.Auth.isGoogleAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|google.ts|]]
    else return []

genAuthKeycloak :: AS.Auth.Auth -> Generator [FileDraft]
genAuthKeycloak auth =
  if AS.Auth.isKeycloakAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|keycloak.ts|]]
    else return []

genAuthGitHub :: AS.Auth.Auth -> Generator [FileDraft]
genAuthGitHub auth =
  if AS.Auth.isGitHubAuthEnabled auth
    then sequence [genClientAuthFileCopy [relfile|github.ts|]]
    else return []

clientAuthDirInUserCoreTemplatesDir :: Path' (Rel UserCoreTemplatesDir) Dir'
clientAuthDirInUserCoreTemplatesDir = [reldir|client/auth|]

genClientAuthFileCopy :: Path' Rel' File' -> Generator FileDraft
genClientAuthFileCopy =
  return . mkTmplFd . (clientAuthDirInUserCoreTemplatesDir </>)
