module Wasp.Generator.SdkGenerator.Client.RouterGenerator
  ( genClientRouterApi,
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
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    genFileCopy,
    mkTmplFdWithData,
  )
import Wasp.Util.Js (makeJsStringLiteral)
import qualified Wasp.Util.WebRouterPath as WebRouterPath

genClientRouterApi :: AppSpec -> Generator [FileDraft]
genClientRouterApi spec =
  sequence
    [ genRoutesTs spec,
      genFileCopyInClientRouter [relfile|index.ts|],
      genFileCopyInClientRouter [relfile|Link.tsx|],
      genFileCopyInClientRouter [relfile|NavLink.tsx|]
    ]

-- | The route map, in its own module without the React components, so code
-- that doesn't render (like the server) can import it.
genRoutesTs :: AppSpec -> Generator FileDraft
genRoutesTs spec =
  return $
    mkTmplFdWithData
      (clientRouterDirInSdkTemplatesDir </> [relfile|routes.ts|])
      tmplData
  where
    tmplData = object ["routes" .= map createRouteTemplateData (AS.getRoutes spec)]

createRouteTemplateData :: AS.Route.Route -> Aeson.Value
createRouteTemplateData route =
  object
    [ "name" .= route.name,
      "urlPath" .= makeJsStringLiteral path,
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
    mapPathParamToJson (WebRouterPath.RequiredParamSegment paramName) = object ["name" .= makeJsStringLiteral paramName, "isOptional" .= False]
    mapPathParamToJson (WebRouterPath.OptionalParamSegment paramName) = object ["name" .= makeJsStringLiteral paramName, "isOptional" .= True]

clientRouterDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
clientRouterDirInSdkTemplatesDir = [reldir|client/router|]

genFileCopyInClientRouter :: Path' Rel' File' -> Generator FileDraft
genFileCopyInClientRouter =
  genFileCopy . (clientRouterDirInSdkTemplatesDir </>)
