module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genNewClientAuth,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    enabledAuthMethodsJson,
    enabledOAuthProviders,
    isEmailEnabled,
    isUsernameAndPasswordEnabled,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
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
        <++> genOAuthClientFiles auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|index.ts|])
      (enabledAuthMethodsJson auth)

genAuthUi :: AS.Auth.Auth -> Generator FileDraft
genAuthUi auth =
  return $
    mkTmplFdWithData
      (clientAuthDirInSdkTemplatesDir </> [relfile|ui.ts|])
      (enabledAuthMethodsJson auth)

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if isEmailEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|email.ts|]]
    else return []

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if isUsernameAndPasswordEnabled auth
    then sequence [genFileCopyInClientAuth [relfile|username.ts|]]
    else return []

genOAuthClientFiles :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthClientFiles auth =
  sequence
    [ genFileCopyInClientAuth (fromJust $ SP.parseRelFile $ slug spec ++ ".ts")
      | (spec, _) <- enabledOAuthProviders auth
    ]

clientAuthDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
clientAuthDirInSdkTemplatesDir = [reldir|client/auth|]

genFileCopyInClientAuth :: Path' Rel' File' -> Generator FileDraft
genFileCopyInClientAuth =
  genFileCopy . (clientAuthDirInSdkTemplatesDir </>)
