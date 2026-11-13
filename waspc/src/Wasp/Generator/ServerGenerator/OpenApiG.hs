module Wasp.Generator.ServerGenerator.OpenApiG
  ( genOpenApi,
    makeOpenApiDocument,
    expressPathToOpenApiPaths,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import Data.Char (isAlphaNum, toLower)
import Data.List (nub)
import qualified Data.Map as Map
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

genOpenApi :: AppSpec -> Generator [FileDraft]
genOpenApi spec
  | isOpenApiEnabled spec =
      return
        [ C.mkTmplFdWithDstAndData
            (C.asTmplFile [relfile|src/routes/openapi.ts|])
            (C.asServerFile [relfile|src/routes/openapi.ts|])
            (Just $ object ["openApiDocument" .= TLE.decodeUtf8 (encodeOpenApiDocument $ makeOpenApiDocument spec)])
        ]
  | otherwise = return []
  where
    encodeOpenApiDocument = Aeson.Pretty.encodePretty' Aeson.Pretty.defConfig {Aeson.Pretty.confIndent = Aeson.Pretty.Spaces 2}

-- | Describes the app's custom `api` routes as an OpenAPI 3.1 document.
-- Wasp doesn't know the shape of request and response bodies, so the document
-- only lists paths, methods, path parameters, and authentication.
-- The server adds the `servers` field at runtime, since it depends on env vars.
makeOpenApiDocument :: AppSpec -> Aeson.Value
makeOpenApiDocument spec =
  object $
    [ "openapi" .= ("3.1.0" :: String),
      "info"
        .= object
          [ "title" .= (getApp spec).title,
            "version" .= fromMaybe "0.0.0" (Npm.PackageJson.version $ AS.packageJson spec)
          ],
      "paths" .= paths
    ]
      ++ [ "components" .= object ["securitySchemes" .= object [bearerAuthSchemeName .= bearerAuthScheme]]
         | anyApiUsesAuth
         ]
  where
    apis = AS.getApis spec

    -- Express answers with the first matching route, so the first api wins
    -- when two of them describe the same path and method.
    paths :: Map.Map String (Map.Map String Aeson.Value)
    paths =
      Map.fromListWith (flip Map.union) $
        [ (openApiPath, Map.singleton method operation)
        | api <- apis,
          (openApiPath, method, operation) <- getApiOperations api
        ]

    getApiOperations :: Api.Api -> [(String, String, Aeson.Value)]
    getApiOperations api =
      [ (openApiPath, method, makeOperation api operationId pathParams)
      | (pathIndex, (openApiPath, pathParams)) <- zip [1 :: Int ..] openApiPaths,
        method <- methods,
        let operationId =
              concat $
                api.name
                  : ["_" ++ method | length methods > 1]
                  ++ ["_" ++ show pathIndex | length openApiPaths > 1]
      ]
      where
        openApiPaths = expressPathToOpenApiPaths (Api.path api)
        methods = getOpenApiMethods (Api.method api)

    makeOperation :: Api.Api -> String -> [String] -> Aeson.Value
    makeOperation api operationId pathParams =
      object $
        [ "operationId" .= operationId,
          "responses" .= object ["default" .= object ["description" .= ("Response from the API's handler." :: String)]]
        ]
          ++ ["parameters" .= map makePathParameter pathParams | not (null pathParams)]
          ++ ["security" .= getOperationSecurity api | anyApiUsesAuth]

    -- Authentication is optional: Wasp sets `context.user` when the request
    -- has a valid session, and the handler decides what to do. Apis without
    -- auth list no requirements, so they don't look like we forgot them.
    getOperationSecurity :: Api.Api -> [Aeson.Value]
    getOperationSecurity api
      | isAuthEnabledForApi spec api = [object [bearerAuthSchemeName .= ([] :: [String])], object []]
      | otherwise = []

    anyApiUsesAuth = any (isAuthEnabledForApi spec) apis

    makePathParameter :: String -> Aeson.Value
    makePathParameter name =
      object
        [ "name" .= name,
          "in" .= ("path" :: String),
          "required" .= True,
          "schema" .= object ["type" .= ("string" :: String)]
        ]

bearerAuthSchemeName :: Aeson.Key
bearerAuthSchemeName = "bearerAuth"

bearerAuthScheme :: Aeson.Value
bearerAuthScheme =
  object
    [ "type" .= ("http" :: String),
      "scheme" .= ("bearer" :: String),
      "description" .= ("The session ID of a logged-in user." :: String)
    ]

getOpenApiMethods :: Api.HttpMethod -> [String]
getOpenApiMethods Api.ALL = map (map toLower . show) [Api.GET, Api.POST, Api.PUT, Api.PATCH, Api.DELETE, Api.HEAD]
getOpenApiMethods method = [map toLower $ show method]

data PathToken
  = Literal Char
  | Param String
  | OptionalGroup [PathToken]

-- | Converts an Express route path to OpenAPI path templates, together with the
-- names of their path parameters. Express paths with optional groups (e.g.
-- @/users{/:id}@) describe several OpenAPI paths, one per combination of the
-- groups being present or absent.
--
-- OpenAPI has no syntax for wildcards (@*name@), so they become regular path
-- parameters.
expressPathToOpenApiPaths :: String -> [(String, [String])]
expressPathToOpenApiPaths expressPath =
  [ (renderPath tokens, nub [name | Param name <- tokens])
  | tokens <- expandOptionalGroups $ fst $ parseTokens expressPath
  ]
  where
    renderPath tokens = case concatMap renderToken tokens of
      "" -> "/"
      openApiPath -> openApiPath

    renderToken (Literal char) = [char]
    renderToken (Param name) = "{" ++ name ++ "}"
    renderToken (OptionalGroup _) = error "Optional groups are expanded before rendering."

    expandOptionalGroups :: [PathToken] -> [[PathToken]]
    expandOptionalGroups [] = [[]]
    expandOptionalGroups (OptionalGroup groupTokens : rest) =
      [group ++ restTokens | group <- [] : expandOptionalGroups groupTokens, restTokens <- expandOptionalGroups rest]
    expandOptionalGroups (token : rest) = (token :) <$> expandOptionalGroups rest

-- | Parses tokens until the end of the path or a closing brace, and returns the
-- remaining input.
parseTokens :: String -> ([PathToken], String)
parseTokens "" = ([], "")
parseTokens input@('}' : _) = ([], input)
parseTokens ('{' : rest) =
  let (groupTokens, afterGroup) = parseTokens rest
   in prependToken (OptionalGroup groupTokens) (parseTokens $ drop 1 afterGroup)
parseTokens (char : rest)
  | char `elem` [':', '*'] =
      let (name, afterName) = parseParamName rest
       in prependToken (Param name) (parseTokens afterName)
parseTokens ('\\' : char : rest) = prependToken (Literal char) (parseTokens rest)
parseTokens (char : rest) = prependToken (Literal char) (parseTokens rest)

parseParamName :: String -> (String, String)
parseParamName ('"' : rest) = drop 1 <$> break (== '"') rest
parseParamName input = span isParamNameChar input
  where
    isParamNameChar char = isAlphaNum char || char `elem` ['_', '$']

prependToken :: PathToken -> ([PathToken], String) -> ([PathToken], String)
prependToken token (tokens, rest) = (token : tokens, rest)
