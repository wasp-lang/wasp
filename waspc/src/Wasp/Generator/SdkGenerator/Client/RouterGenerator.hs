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
import Wasp.Util.WebRouterPath (Param (Optional, Required), extractPathParams)

genNewClientRouterApi :: AppSpec -> Generator [FileDraft]
genNewClientRouterApi spec =
  sequence
    [ genRouterTsx spec,
      genFileCopy [relfile|client/router/types.ts|],
      genFileCopy [relfile|client/router/linkHelpers.ts|],
      genFileCopy [relfile|client/router/Link.tsx|]
    ]
  where
    genFileCopy = return . C.mkTmplFd

genRouterTsx :: AppSpec -> Generator FileDraft
genRouterTsx spec = return $ C.mkTmplFdWithData [relfile|client/router/index.ts|] tmplData
  where
    tmplData =
      object ["routes" .= map createRouteTemplateData (AS.getRoutes spec)]

createRouteTemplateData :: (String, AS.Route.Route) -> Aeson.Value
createRouteTemplateData (name, route) =
  object
    [ "name" .= name,
      "urlPath" .= path,
      "urlParams" .= map mapPathParamToJson urlParams,
      "hasUrlParams" .= (not . null $ urlParams)
    ]
  where
    path = AS.Route.path route

    urlParams = extractPathParams path

    mapPathParamToJson :: Param -> Aeson.Value
    mapPathParamToJson (Required paramName) = object ["name" .= paramName, "isOptional" .= False]
    mapPathParamToJson (Optional paramName) = object ["name" .= paramName, "isOptional" .= True]
