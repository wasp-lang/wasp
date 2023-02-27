module Wasp.Generator.ServerGenerator.ApiRoutesG
  ( genApiRoutes,
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

genApiRoutes :: AppSpec -> Generator FileDraft
genApiRoutes spec =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    apis = map snd $ AS.getApis spec
    tmplData = object ["apiRoutes" .= map getRouteData apis]
    tmplFile = C.asTmplFile [relfile|src/routes/apis/index.js|]
    dstFile = SP.castRel tmplFile :: Path' (Rel ServerRootDir) File'

    getRouteData :: Api -> Aeson.Value
    getRouteData api =
      let (jsImportStmt, jsImportIdentifier) = getJsImportStmtAndIdentifier relPathFromApisDirToServerSrcDir (Api.fn api)
       in object
            [ "routeMethod" .= map toLower (show $ Api.method api),
              "routePath" .= Api.path api,
              "importStatement" .= jsImportStmt,
              "importIdentifier" .= jsImportIdentifier,
              "entities" .= allEntities
            ]
      where
        allEntities = maybe [] (map (makeJsonWithEntityData . AS.refName)) (Api.entities api)

    relPathFromApisDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromApisDirToServerSrcDir = [reldirP|../..|]
