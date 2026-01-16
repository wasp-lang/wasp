{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.ServerG
  ( genServer,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd, mkTmplFdWithData)
import Wasp.Generator.SdkGenerator.Core.Server.AuthG (genServerAuth)
import Wasp.Generator.SdkGenerator.Core.Server.EmailG (genServerEmail)
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  return
    [ mkTmplFd [relfile|server/HttpError.ts|],
      mkTmplFd [relfile|server/types/index.ts|],
      mkTmplFd [relfile|server/jobs/core/job.ts|]
    ]
    <++> genServerMiddleware
    <++> genServerTaggedEntities spec
    <++> genServerAuth spec
    <++> genServerEmail spec

genServerMiddleware :: Generator [FileDraft]
genServerMiddleware =
  return
    [ mkTmplFd [relfile|server/middleware/index.ts|],
      mkTmplFd [relfile|server/middleware/globalMiddleware.ts|]
    ]

genServerTaggedEntities :: AppSpec -> Generator [FileDraft]
genServerTaggedEntities spec =
  return [mkTmplFdWithData tmplFile tmplData]
  where
    tmplFile = [relfile|server/_types/taggedEntities.ts|]
    tmplData = object ["entities" .= allEntities]

    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec
