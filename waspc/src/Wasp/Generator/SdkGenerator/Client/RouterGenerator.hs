module Wasp.Generator.SdkGenerator.Client.RouterGenerator
  ( genNewClientRouterApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genNewClientRouterApi :: AppSpec -> Generator [FileDraft]
genNewClientRouterApi spec =
  sequence
    [ genIndexTs spec,
      genFileCopy [relfile|client/router/types.ts|],
      genFileCopy [relfile|client/router/linkHelpers.ts|],
      genFileCopy [relfile|client/router/Link.tsx|]
    ]
  where
    genFileCopy = return . C.mkTmplFd

genIndexTs :: AppSpec -> Generator FileDraft
genIndexTs spec =
  return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|client/router/index.ts|]
    tmplData = object ["routes" .= map createRouteTemplateData (AS.getRoutes spec)]

createRouteTemplateData :: (String, AS.Route.Route) -> Aeson.Value
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

    mapPathParamToJson :: WebRouterPath.ParamSegment -> Aeson.Value
    mapPathParamToJson (WebRouterPath.RequiredParamSegment paramName) = object ["name" .= paramName, "isOptional" .= False]
    mapPathParamToJson (WebRouterPath.OptionalParamSegment paramName) = object ["name" .= paramName, "isOptional" .= True]
