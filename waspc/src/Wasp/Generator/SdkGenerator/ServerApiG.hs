module Wasp.Generator.SdkGenerator.ServerApiG
  ( genServerApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import Data.Maybe (fromMaybe)
import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec, getApis)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as Api
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
import Wasp.Generator.ServerGenerator.ApiRoutesG (getApiEntitiesObject, isAuthEnabledForApi)
import Wasp.Util (toUpperFirst)

genServerApi :: AppSpec -> Generator [FileDraft]
genServerApi spec =
  if areThereAnyCustomApiRoutes
    then
      sequence
        [ genIndexTsWithApiRoutes spec
        ]
    else return []
  where
    areThereAnyCustomApiRoutes = not . null $ getApis spec

genIndexTsWithApiRoutes :: AppSpec -> Generator FileDraft
genIndexTsWithApiRoutes spec =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    namedApis = AS.getApis spec
    apis = snd <$> namedApis
    tmplData =
      object
        [ "apiRoutes" .= map getTmplData namedApis,
          "shouldImportAuthenticatedApi" .= any usesAuth apis,
          "shouldImportNonAuthenticatedApi" .= not (all usesAuth apis),
          "allEntities" .= nub (concatMap getApiEntitiesObject apis)
        ]
    usesAuth = fromMaybe (isAuthEnabledGlobally spec) . Api.auth

    tmplFile :: Path' (Rel SdkTemplatesProjectDir) File'
    tmplFile = [relfile|server/api/index.ts|]

    getTmplData :: (String, Api.Api) -> Aeson.Value
    getTmplData (name, api) =
      object
        [ "typeName" .= toUpperFirst name,
          "entities" .= getApiEntitiesObject api,
          "usesAuth" .= isAuthEnabledForApi spec api
        ]

    isAuthEnabledGlobally :: AppSpec -> Bool
    isAuthEnabledGlobally = isAuthEnabled
