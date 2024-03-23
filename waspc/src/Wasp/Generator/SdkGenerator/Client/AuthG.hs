module Wasp.Generator.SdkGenerator.Client.AuthG
  ( genNewClientAuth,
  )
where

import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.App.Auth.AuthMethods (AuthMethod (..))
import qualified Wasp.AppSpec.App.Auth.IsEnabled as AS.Auth.IsEnabled
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
        <++> genAuthHelpers auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/auth/index.ts|]
      tmplData
  where
    tmplData = AuthProviders.getAuthProvidersJson auth

genAuthUI :: AS.Auth.Auth -> Generator FileDraft
genAuthUI auth =
  return $
    C.mkTmplFdWithData
      [relfile|client/auth/ui.ts|]
      tmplData
  where
    tmplData = AuthProviders.getAuthProvidersJson auth

genAuthHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genAuthHelpers auth = return $ concatMap genAuthHelper [minBound .. maxBound :: AuthMethod]
  where
    genAuthHelper :: AuthMethod -> [FileDraft]
    genAuthHelper authMethod =
      if AS.Auth.IsEnabled.isAuthMethodEnabled authMethod auth
        then [genHelperForAuthMethod authMethod]
        else []

genHelperForAuthMethod :: AuthMethod -> FileDraft
genHelperForAuthMethod Google = genFileCopy [relfile|client/auth/google.ts|]
genHelperForAuthMethod Keycloak = genFileCopy [relfile|client/auth/keycloak.ts|]
genHelperForAuthMethod GitHub = genFileCopy [relfile|client/auth/github.ts|]
genHelperForAuthMethod Email = genFileCopy [relfile|client/auth/email.ts|]
genHelperForAuthMethod UsernameAndPassword = genFileCopy [relfile|client/auth/username.ts|]

genFileCopy :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
genFileCopy = C.mkTmplFd
