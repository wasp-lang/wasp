module Wasp.Generator.SdkGenerator.RouterGenerator
  ( genRouter,
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

genRouter :: AppSpec -> Generator [FileDraft]
genRouter spec =
  sequence
    [ genRouterTsx spec,
      genFileCopy [relfile|router/types.ts|],
      genFileCopy [relfile|router/linkHelpers.ts|],
      genFileCopy [relfile|router/Link.tsx|]
    ]
  where
    genFileCopy = return . C.mkTmplFd

genRouterTsx :: AppSpec -> Generator FileDraft
genRouterTsx spec = return $ C.mkTmplFdWithData [relfile|router/index.ts|] tmplData
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
