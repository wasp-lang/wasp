{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.EntitiesG
  ( genEntities,
  )
where

import Data.Aeson (KeyValue ((.=)), object, (.=))
import Data.Maybe (isJust)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsonWithEntityData)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFdWithData)

genEntities :: AppSpec -> Generator [FileDraft]
genEntities spec = sequence [genEntitiesIndex spec]

genEntitiesIndex :: AppSpec -> Generator FileDraft
genEntitiesIndex spec =
  return $
    mkTmplFdWithData
      [relfile|entities/index.ts|]
      ( object
          [ "entities" .= allEntities,
            "isAuthEnabled" .= isJust maybeUserEntityName,
            "authEntityName" .= DbAuth.authEntityName,
            "authIdentityEntityName" .= DbAuth.authIdentityEntityName
          ]
      )
  where
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec
    maybeUserEntityName = AS.refName . AS.App.Auth.userEntity <$> (snd $ getApp spec).auth
