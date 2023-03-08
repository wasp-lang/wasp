module Wasp.Generator.ServerGenerator.ApiRoutesG
  ( genApis,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldirP, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getApis)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as App.Auth
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (ServerRootDir, makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (getJsImportStmtAndIdentifier)
import Wasp.Util (toUpperFirst)

genApis :: AppSpec -> Generator [FileDraft]
genApis spec =
  if areApisInUse
    then
      sequence
        [ genApiRoutes spec,
          genApiTypes spec
        ]
    else return []
  where
    areApisInUse = not . null $ getApis spec

genApiRoutes :: AppSpec -> Generator FileDraft
genApiRoutes spec =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    apis = map snd $ AS.getApis spec
    tmplData =
      object
        [ "apiRoutes" .= map getTmplData apis,
          "isAuthEnabled" .= isAuthEnabledGlobally spec,
          "userEntityName" .= maybe "" (AS.refName . App.Auth.userEntity) (App.auth $ snd $ getApp spec)
        ]
    tmplFile = C.asTmplFile [relfile|src/routes/apis/index.ts|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getTmplData :: Api.Api -> Aeson.Value
    getTmplData api =
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier relPathFromApisRoutesToServerSrcDir (Api.fn api)
       in object
            [ "routeMethod" .= map toLower (show $ Api.method api),
              "routePath" .= Api.path api,
              "importStatement" .= jsImportStmt,
              "importIdentifier" .= jsImportIdentifier,
              "entities" .= getApiEntitiesObject api,
              "usesAuth" .= isAuthEnabledForApi spec api
            ]
      where
        relPathFromApisRoutesToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
        relPathFromApisRoutesToServerSrcDir = [reldirP|../..|]

genApiTypes :: AppSpec -> Generator FileDraft
genApiTypes spec =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
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
    tmplFile = C.asTmplFile [relfile|src/apis/types.ts|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getTmplData :: (String, Api.Api) -> Aeson.Value
    getTmplData (name, api) =
      object
        [ "typeName" .= toUpperFirst name,
          "entities" .= getApiEntitiesObject api,
          "usesAuth" .= isAuthEnabledForApi spec api
        ]

getApiEntitiesObject :: Api.Api -> [Aeson.Value]
getApiEntitiesObject api = maybe [] (map (makeJsonWithEntityData . AS.refName)) (Api.entities api)

isAuthEnabledGlobally :: AppSpec -> Bool
isAuthEnabledGlobally = isAuthEnabled

isAuthEnabledForApi :: AppSpec -> Api.Api -> Bool
isAuthEnabledForApi spec api = fromMaybe (isAuthEnabled spec) (Api.auth api)
