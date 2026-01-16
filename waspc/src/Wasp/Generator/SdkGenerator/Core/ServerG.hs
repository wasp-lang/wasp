{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.ServerG
  ( genServer,
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
import Wasp.Generator.SdkGenerator.Core.Server.EmailG (genServerEmail)
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  return
    [ mkTmplFd [relfile|server/HttpError.ts|],
      mkTmplFd [relfile|server/types/index.ts|],
      mkTmplFd [relfile|server/jobs/core/job.ts|],
      mkTmplFd [relfile|server/middleware/globalMiddleware.ts|]
    ]
    <++> genServerAuth spec
    <++> genServerEmail spec

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
