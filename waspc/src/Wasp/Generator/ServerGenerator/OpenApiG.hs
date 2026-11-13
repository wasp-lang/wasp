module Wasp.Generator.ServerGenerator.OpenApiG
  ( genOpenApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy.Encoding as TLE
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp, isOpenApiEnabled)
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.ApiRoutesG (isAuthEnabledForApi)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Util.Js (makeJsStringLiteral)

-- | The server builds the OpenAPI document at runtime from the data we pass
-- here (see `makeOpenApiDocument` in `@wasp/lib-sdk-core`).
genOpenApi :: AppSpec -> Generator [FileDraft]
genOpenApi spec
  | isOpenApiEnabled spec =
      return
        [ C.mkTmplFdWithDstAndData
            (C.asTmplFile [relfile|src/routes/openapi.ts|])
            (C.asServerFile [relfile|src/routes/openapi.ts|])
            (Just tmplData)
        ]
  | otherwise = return []
  where
    tmplData =
      object
        [ "title" .= makeJsStringLiteral (getApp spec).title,
          "version" .= makeJsStringLiteral (fromMaybe "0.0.0" $ Npm.PackageJson.version $ AS.packageJson spec),
          "apis" .= map (TLE.decodeUtf8 . Aeson.encode . getApiData) (AS.getApis spec)
        ]

    getApiData :: Api.Api -> Aeson.Value
    getApiData api =
      object
        [ "name" .= api.name,
          "method" .= show (Api.method api),
          "path" .= Api.path api,
          "usesAuth" .= isAuthEnabledForApi spec api
        ]
