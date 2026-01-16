{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.Server.AuthG
  ( genServerAuth,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (genConditionally)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd)
import Wasp.Util ((<++>))

genServerAuth :: AppSpec -> Generator [FileDraft]
genServerAuth spec =
  case maybeAuth of
    Nothing -> return []
    Just auth ->
      genServerAuthUsername auth
        <++> genServerAuthOAuth
  where
    maybeAuth = (snd $ getApp spec).auth

genServerAuthUsername :: AS.Auth.Auth -> Generator [FileDraft]
genServerAuthUsername auth =
  genConditionally isUsernameAndPasswordAuthEnabled [mkTmplFd [relfile|server/auth/username.ts|]]
  where
    isUsernameAndPasswordAuthEnabled = AS.Auth.isUsernameAndPasswordAuthEnabled auth

genServerAuthOAuth :: Generator [FileDraft]
genServerAuthOAuth =
  return [mkTmplFd [relfile|server/auth/oauth/provider.ts|]]
