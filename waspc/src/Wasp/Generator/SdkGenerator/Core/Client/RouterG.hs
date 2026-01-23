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
import Wasp.Generator.SdkGenerator.Core.Common (CoreTemplatesDir, mkTmplFd, mkTmplFdWithData)
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genClientRouter :: AppSpec -> Generator [FileDraft]
genClientRouter spec =
  sequence
    [ genClientRouterIndex spec,
      genClientRouterFileCopy [relfile|Link.tsx|],
      genClientRouterFileCopy [relfile|linkHelpers.ts|],
      genClientRouterFileCopy [relfile|types.ts|]
    ]

genClientRouterIndex :: AppSpec -> Generator FileDraft
genClientRouterIndex spec =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = clientRouterDirInUserCoreTemplatesDir </> [relfile|index.ts|]
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

clientRouterDirInUserCoreTemplatesDir :: Path' (Rel CoreTemplatesDir) Dir'
clientRouterDirInUserCoreTemplatesDir = [reldir|client/router|]

genClientRouterFileCopy :: Path' Rel' File' -> Generator FileDraft
genClientRouterFileCopy =
  return . mkTmplFd . (clientRouterDirInUserCoreTemplatesDir </>)
