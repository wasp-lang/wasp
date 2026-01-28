module Wasp.Generator.SdkGenerator.Core.Client.RouterG
  ( genClientRouter,
  )
where

import Data.Aeson (KeyValue ((.=)), Value, object)
import StrongPath (reldir, relfile, (</>))
import StrongPath.Types
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (SdkTemplatesCoreProjectDir, mkTmplFd, mkTmplFdWithData)
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genClientRouter :: AppSpec -> Generator [FileDraft]
genClientRouter spec =
  sequence
    [ genClientRouterIndex spec,
      genFileCopyInClientRouter [relfile|types.ts|],
      genFileCopyInClientRouter [relfile|linkHelpers.ts|],
      genFileCopyInClientRouter [relfile|Link.tsx|]
    ]

genClientRouterIndex :: AppSpec -> Generator FileDraft
genClientRouterIndex spec =
  return $ mkTmplFdWithData (clientRouterDirInSdkTemplatesCoreProjectDir </> [relfile|index.ts|]) tmplData
  where
    tmplData = object ["routes" .= map createRouteTemplateData (AS.getRoutes spec)]

createRouteTemplateData :: (String, AS.Route.Route) -> Value
createRouteTemplateData (name, route) =
  object
    [ "name" .= name,
      "urlPath" .= path,
      "urlParams" .= map mapPathParamToJson urlParams,
      "hasUrlParams" .= (not . null $ urlParams),
      "hasOptionalStaticSegments" .= (not . null $ optionalStaticSegments)
    ]
  where
    path = AS.Route.path route

    routeSegments = WebRouterPath.getRouteSegments path
    urlParams = [param | WebRouterPath.ParamSegment param <- routeSegments]
    optionalStaticSegments = [segment | (WebRouterPath.StaticSegment (WebRouterPath.OptionalStaticSegment segment)) <- routeSegments]

    mapPathParamToJson :: WebRouterPath.ParamSegment -> Value
    mapPathParamToJson (WebRouterPath.RequiredParamSegment paramName) = object ["name" .= paramName, "isOptional" .= False]
    mapPathParamToJson (WebRouterPath.OptionalParamSegment paramName) = object ["name" .= paramName, "isOptional" .= True]

clientRouterDirInSdkTemplatesCoreProjectDir :: Path' (Rel SdkTemplatesCoreProjectDir) Dir'
clientRouterDirInSdkTemplatesCoreProjectDir = [reldir|client/router|]

genFileCopyInClientRouter :: Path' Rel' File' -> Generator FileDraft
genFileCopyInClientRouter =
  return . mkTmplFd . (clientRouterDirInSdkTemplatesCoreProjectDir </>)
