module Wasp.Generator.ServerGenerator.ApiRoutesG
  ( genApis,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldirP, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Api (Api)
import qualified Wasp.AppSpec.Api as Api
import Wasp.Generator.Common (ServerRootDir, makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (getJsImportStmtAndIdentifier)

genApis :: AppSpec -> Generator [FileDraft]
genApis spec =
  sequence
    [ genApiRoutes spec,
      genApiTypes spec
    ]

genApiRoutes :: AppSpec -> Generator FileDraft
genApiRoutes spec =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    apis = map snd $ AS.getApis spec
    tmplData = object ["apiRoutes" .= map getTmplData apis]
    tmplFile = C.asTmplFile [relfile|src/routes/apis/index.js|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getTmplData :: Api -> Aeson.Value
    getTmplData api =
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier relPathFromApisDirToServerSrcDir (Api.fn api)
       in object
            [ "routeMethod" .= map toLower (show $ Api.method . Api.httpRoute $ api),
              "routePath" .= (Api.path . Api.httpRoute $ api),
              "importStatement" .= jsImportStmt,
              "importIdentifier" .= jsImportIdentifier,
              "entities" .= getApiEntitiesObject api
            ]

genApiTypes :: AppSpec -> Generator FileDraft
genApiTypes spec =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    namedApis = AS.getApis spec
    tmplData = object ["apiRoutes" .= map getTmplData namedApis]
    tmplFile = C.asTmplFile [relfile|src/apis/types.ts|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getTmplData :: (String, Api) -> Aeson.Value
    getTmplData (name, api) =
      object
        [ "name" .= name,
          "entities" .= getApiEntitiesObject api
        ]

getApiEntitiesObject :: Api -> [Aeson.Value]
getApiEntitiesObject api = maybe [] (map (makeJsonWithEntityData . AS.refName)) (Api.entities api)

relPathFromApisDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
relPathFromApisDirToServerSrcDir = [reldirP|../..|]
