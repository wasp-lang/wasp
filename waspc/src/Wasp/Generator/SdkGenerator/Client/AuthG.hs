module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genNewClientAuth,
  )
where

import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util ((<++>))

genNewClientAuth :: AppSpec -> Generator [FileDraft]
genNewClientAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genAuthIndex auth,
          genAuthUI auth
        ]
        <++> genAuthEmail auth
        <++> genAuthUsername auth
        <++> genAuthSlack auth
        <++> genAuthDiscord auth
        <++> genAuthGoogle auth
        <++> genAuthKeycloak auth
        <++> genAuthGitHub auth
        <++> genAuthMicrosoftEntra auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/auth/index.ts|]
      tmplData
  where
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthUI :: AS.Auth.Auth -> Generator FileDraft
genAuthUI auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/auth/ui.ts|]
      tmplData
  where
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if AS.Auth.isEmailAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/email.ts|]]
    else return []

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if AS.Auth.isUsernameAndPasswordAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/username.ts|]]
    else return []

genAuthSlack :: AS.Auth.Auth -> Generator [FileDraft]
genAuthSlack auth =
  if AS.Auth.isSlackAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/slack.ts|]]
    else return []

genAuthDiscord :: AS.Auth.Auth -> Generator [FileDraft]
genAuthDiscord auth =
  if AS.Auth.isDiscordAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/discord.ts|]]
    else return []

genAuthGoogle :: AS.Auth.Auth -> Generator [FileDraft]
genAuthGoogle auth =
  if AS.Auth.isGoogleAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/google.ts|]]
    else return []

genAuthKeycloak :: AS.Auth.Auth -> Generator [FileDraft]
genAuthKeycloak auth =
  if AS.Auth.isKeycloakAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/keycloak.ts|]]
    else return []

genAuthGitHub :: AS.Auth.Auth -> Generator [FileDraft]
genAuthGitHub auth =
  if AS.Auth.isGitHubAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/github.ts|]]
    else return []

genAuthMicrosoftEntra :: AS.Auth.Auth -> Generator [FileDraft]
genAuthMicrosoftEntra auth =
  if AS.Auth.isMicrosoftEntraAuthEnabled auth
    then sequence [genFileCopy [relfile|client/auth/microsoftEntra.ts|]]
    else return []

genFileCopy :: Path' (Rel SdkTemplatesDir) File' -> Generator FileDraft
genFileCopy = return . C.mkTmplFd
