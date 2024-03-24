module Wasp.Generator.SdkGenerator.Server.AuthG
  ( genNewServerApi,
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

genNewServerApi :: AppSpec -> Generator [FileDraft]
genNewServerApi spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      sequence
        [ genAuthIndex auth
        ]
        <++> genAuthEmail auth
        <++> genAuthUsername auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

genAuthIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthIndex auth =
  return $
    C.mkTmplFdWithData
      [relfile|server/auth/index.ts|]
      tmplData
  where
    tmplData = AuthProviders.getEnabledAuthProvidersJson auth

genAuthEmail :: AS.Auth.Auth -> Generator [FileDraft]
genAuthEmail auth =
  if AS.Auth.isEmailAuthEnabled auth
    then sequence [genFileCopy [relfile|server/auth/email/index.ts|]]
    else return []

genAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genAuthUsername auth =
  if AS.Auth.isUsernameAndPasswordAuthEnabled auth
    then sequence [genFileCopy [relfile|server/auth/username.ts|]]
    else return []

genFileCopy :: Path' (Rel SdkTemplatesDir) File' -> Generator FileDraft
genFileCopy = return . C.mkTmplFd
