module Wasp.Generator.ServerGenerator.ApiRoutesG
  ( genApis,
    getApiEntitiesObject,
    isAuthEnabledForApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldirP, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getApis)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.ApiNamespace as ApiNamespace
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Common (ServerRootDir, makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)

genApis :: AppSpec -> Generator [FileDraft]
genApis spec =
  if areThereAnyCustomApiRoutes
    then
      sequence
        [ genApiRoutes spec
        ]
    else return []
  where
    areThereAnyCustomApiRoutes = not . null $ getApis spec

genApiRoutes :: AppSpec -> Generator FileDraft
genApiRoutes spec = do
  apiRoutes <- mapM getApiRoutesTmplData namedApis
  apiNamespaces <- mapM getNamespaceTmplData namedNamespaces
  let tmplData =
        object
          [ "apiRoutes" .= apiRoutes,
            "apiNamespaces" .= apiNamespaces,
            "isAuthEnabled" .= isAuthEnabledGlobally spec
          ]
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    namedApis = AS.getApis spec
    namedNamespaces = AS.getApiNamespaces spec
    tmplFile = C.asTmplFile [relfile|src/routes/apis/index.ts|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getNamespaceTmplData :: (String, ApiNamespace.ApiNamespace) -> Generator Aeson.Value
    getNamespaceTmplData (_namespaceName, namespace) = do
      namespaceMiddlewareConfigFn <- extImportToImportJson relPathFromApisRoutesToServerSrcDir $ Just (ApiNamespace.middlewareConfigFn namespace)

      return $
        object
          [ "namespacePath" .= ApiNamespace.path namespace,
            "namespaceMiddlewareConfigFn" .= namespaceMiddlewareConfigFn
          ]

    getApiRoutesTmplData :: (String, Api.Api) -> Generator Aeson.Value
    getApiRoutesTmplData (apiName, api) = do
      apiRouteFn <- extImportToImportJson relPathFromApisRoutesToServerSrcDir $ Just (Api.fn api)
      routeMiddlewareConfigFn <- extImportToImportJson relPathFromApisRoutesToServerSrcDir $ Api.middlewareConfigFn api

      return $
        object
          [ "routeMethod" .= map toLower (show $ Api.method api),
            "routePath" .= Api.path api,
            "apiRouteFn" .= apiRouteFn,
            "entities" .= getApiEntitiesObject api,
            "usesAuth" .= isAuthEnabledForApi spec api,
            "routeMiddlewareConfigFn" .= routeMiddlewareConfigFn,
            "apiName" .= apiName
          ]

relPathFromApisRoutesToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
relPathFromApisRoutesToServerSrcDir = [reldirP|../..|]

getApiEntitiesObject :: Api.Api -> [Aeson.Value]
getApiEntitiesObject api = maybe [] (map (makeJsonWithEntityData . AS.refName)) (Api.entities api)

isAuthEnabledGlobally :: AppSpec -> Bool
isAuthEnabledGlobally = isAuthEnabled

isAuthEnabledForApi :: AppSpec -> Api.Api -> Bool
isAuthEnabledForApi spec api = fromMaybe (isAuthEnabled spec) (Api.auth api)
