module Wasp.Generator.ServerGenerator.ApiRoutesG
  ( genApis,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldirP, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getApis)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.Namespace as N
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Common (ServerRootDir, makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (getAliasedJsImportStmtAndIdentifier)
import Wasp.Util (toUpperFirst)

genApis :: AppSpec -> Generator [FileDraft]
genApis spec =
  if areThereAnyCustomApiRoutes
    then
      sequence
        [ genApiRoutes spec,
          genApiTypes spec
        ]
    else return []
  where
    areThereAnyCustomApiRoutes = not . null $ getApis spec

genApiRoutes :: AppSpec -> Generator FileDraft
genApiRoutes spec =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    namedApis = AS.getApis spec
    namedNamespaces = AS.getNamespaces spec
    tmplData =
      object
        [ "apiRoutes" .= map getApiRoutesTmplData namedApis,
          "namespaces" .= map getNamespaceTmplData namedNamespaces,
          "isAuthEnabled" .= isAuthEnabledGlobally spec
        ]
    tmplFile = C.asTmplFile [relfile|src/routes/apis/index.ts|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getNamespaceTmplData :: (String, N.Namespace) -> Aeson.Value
    getNamespaceTmplData (namespaceName, namespace) =
      object
        [ "namespacePath" .= N.path namespace,
          "namespaceMiddlewareConfigFnImportStatement" .= middlewareConfigFnImport,
          "namespaceMiddlewareConfigFnImportAlias" .= middlewareConfigFnAlias
        ]
      where
        namespaceConfigFnAlias = "_wasp" ++ namespaceName ++ "namespaceMiddlewareConfigFn"
        (middlewareConfigFnImport, middlewareConfigFnAlias) = getAliasedJsImportStmtAndIdentifier namespaceConfigFnAlias relPathFromApisRoutesToServerSrcDir (N.middlewareConfigFn namespace)

    getApiRoutesTmplData :: (String, Api.Api) -> Aeson.Value
    getApiRoutesTmplData (apiName, api) =
      object
        [ "routeMethod" .= map toLower (show $ Api.method api),
          "routePath" .= Api.path api,
          "importStatement" .= jsImportStmt,
          "importIdentifier" .= jsImportIdentifier,
          "entities" .= getApiEntitiesObject api,
          "usesAuth" .= isAuthEnabledForApi spec api,
          "routeMiddlewareConfigFn" .= middlewareConfigFnTmplData,
          "apiName" .= apiName
        ]
      where
        (jsImportStmt, jsImportIdentifier) = getAliasedJsImportStmtAndIdentifier ("_wasp" ++ apiName ++ "fn") relPathFromApisRoutesToServerSrcDir (Api.fn api)

        middlewareConfigFnTmplData :: Aeson.Value
        middlewareConfigFnTmplData =
          let middlewareConfigFnAlias = "_wasp" ++ apiName ++ "middlewareConfigFn"
              maybeMiddlewareConfigFnImport = getAliasedJsImportStmtAndIdentifier middlewareConfigFnAlias relPathFromApisRoutesToServerSrcDir <$> Api.middlewareConfigFn api
           in object
                [ "isDefined" .= isJust maybeMiddlewareConfigFnImport,
                  "importStatement" .= maybe "" fst maybeMiddlewareConfigFnImport,
                  "importAlias" .= middlewareConfigFnAlias
                ]

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
