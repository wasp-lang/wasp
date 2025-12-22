module Wasp.Generator.SdkGenerator.Client.RouterGenerator
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
import Wasp.Generator.SdkGenerator.Client.Common
import Wasp.Generator.SdkGenerator.Common
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genNewClientRouterApi :: AppSpec -> Generator [FileDraft]
genNewClientRouterApi spec =
  sequence
    [ genIndexTs spec,
      genClientRouterFileCopy SdkUserCoreProject [relfile|types.ts|],
      genClientRouterFileCopy SdkUserCoreProject [relfile|linkHelpers.ts|],
      genClientRouterFileCopy SdkUserCoreProject [relfile|Link.tsx|]
    ]

genIndexTs :: AppSpec -> Generator FileDraft
genIndexTs spec =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = clientRouterDirInSdkTemplatesProjectDir </> [relfile|index.ts|]
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

clientRouterDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
clientRouterDirInSdkTemplatesProjectDir = clientTemplatesDirInSdkTemplatesDir </> [reldir|router|]

genClientRouterFileCopy :: SdkProject -> Path' Rel' File' -> Generator FileDraft
genClientRouterFileCopy sdkProject =
  return . makeSdkProjectTmplFd sdkProject . (clientRouterDirInSdkTemplatesProjectDir </>)
