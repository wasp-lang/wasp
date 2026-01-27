module Wasp.Generator.SdkGenerator.UserCore.Client.RouterGenerator
  ( genNewClientRouterApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( SdkTemplatesUserCoreProjectDir,
    mkTmplFd,
    mkTmplFdWithData,
  )
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genNewClientRouterApi :: AppSpec -> Generator [FileDraft]
genNewClientRouterApi spec =
  sequence
    [ genIndexTs spec,
      genFileCopyInClientRouter [relfile|types.ts|],
      genFileCopyInClientRouter [relfile|linkHelpers.ts|],
      genFileCopyInClientRouter [relfile|Link.tsx|]
    ]

genIndexTs :: AppSpec -> Generator FileDraft
genIndexTs spec =
  return $ mkTmplFdWithData (clientRouterDirInSdkTemplatesUserCoreProjectDir </> [relfile|index.ts|]) tmplData
  where
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

clientRouterDirInSdkTemplatesUserCoreProjectDir :: Path' (Rel SdkTemplatesUserCoreProjectDir) Dir'
clientRouterDirInSdkTemplatesUserCoreProjectDir = [reldir|client/router|]

genFileCopyInClientRouter :: Path' Rel' File' -> Generator FileDraft
genFileCopyInClientRouter =
  return . mkTmplFd . (clientRouterDirInSdkTemplatesUserCoreProjectDir </>)
