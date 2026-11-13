module Generator.ServerGenerator.OpenApiGTest where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Aeson.Key
import Data.Maybe (fromJust)
import Fixtures.AppSpec (basicAppSpec)
import qualified StrongPath as SP
import Test.Hspec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import Wasp.Generator.ServerGenerator.OpenApiG (expressPathToOpenApiPaths, makeOpenApiDocument)

spec_expressPathToOpenApiPaths :: Spec
spec_expressPathToOpenApiPaths = do
  it "keeps static paths as they are" $
    expressPathToOpenApiPaths "/foo/bar" `shouldBe` [("/foo/bar", [])]
  it "converts params and wildcards to path parameters" $
    expressPathToOpenApiPaths "/users/:userId/files/*filePath"
      `shouldBe` [("/users/{userId}/files/{filePath}", ["userId", "filePath"])]
  it "supports quoted param names" $
    expressPathToOpenApiPaths "/users/:\"user id\"" `shouldBe` [("/users/{user id}", ["user id"])]
  it "keeps escaped characters as literals" $
    expressPathToOpenApiPaths "/foo\\:bar" `shouldBe` [("/foo:bar", [])]
  it "expands optional groups into one path per combination" $
    expressPathToOpenApiPaths "/users{/:id{/edit}}"
      `shouldBe` [ ("/users", []),
                   ("/users/{id}", ["id"]),
                   ("/users/{id}/edit", ["id"])
                 ]
  it "returns the root path for an empty path" $
    expressPathToOpenApiPaths "{/:lang}" `shouldBe` [("/", []), ("/{lang}", ["lang"])]

spec_makeOpenApiDocument :: Spec
spec_makeOpenApiDocument = do
  it "returns a document with no paths when there are no apis" $
    makeOpenApiDocument (makeSpec [])
      `shouldBe` object
        [ "openapi" .= ("3.1.0" :: String),
          "info" .= object ["title" .= ("Test App" :: String), "version" .= ("0.0.0" :: String)],
          "paths" .= object []
        ]

  it "describes each api route as an operation" $
    makeOpenApiDocument
      ( makeSpec
          [ makeApi "getTask" (AS.Api.GET, "/tasks/:id") Nothing,
            makeApi "updateTask" (AS.Api.PATCH, "/tasks/:id") Nothing,
            makeApi "webhook" (AS.Api.ALL, "/webhook") Nothing
          ]
      )
      `shouldBe` object
        [ "openapi" .= ("3.1.0" :: String),
          "info" .= object ["title" .= ("Test App" :: String), "version" .= ("0.0.0" :: String)],
          "paths"
            .= object
              [ "/tasks/{id}"
                  .= object
                    [ "get" .= operation "getTask" [idParam],
                      "patch" .= operation "updateTask" [idParam]
                    ],
                "/webhook"
                  .= object
                    [ Aeson.Key.fromString method .= operation ("webhook_" ++ method) []
                    | method <- ["get", "post", "put", "patch", "delete", "head"]
                    ]
              ]
        ]

  it "marks apis that use auth with an optional bearer token, and the rest with no security" $
    makeOpenApiDocument (makeSpec [makeApi "me" (AS.Api.GET, "/me") (Just True), makeApi "status" (AS.Api.GET, "/status") Nothing])
      `shouldBe` object
        [ "openapi" .= ("3.1.0" :: String),
          "info" .= object ["title" .= ("Test App" :: String), "version" .= ("0.0.0" :: String)],
          "paths"
            .= object
              [ "/me"
                  .= object
                    [ "get"
                        .= object
                          [ "operationId" .= ("me" :: String),
                            "responses" .= defaultResponses,
                            "security" .= [object ["bearerAuth" .= ([] :: [String])], object []]
                          ]
                    ],
                "/status"
                  .= object
                    [ "get"
                        .= object
                          [ "operationId" .= ("status" :: String),
                            "responses" .= defaultResponses,
                            "security" .= ([] :: [Value])
                          ]
                    ]
              ],
          "components"
            .= object
              [ "securitySchemes"
                  .= object
                    [ "bearerAuth"
                        .= object
                          [ "type" .= ("http" :: String),
                            "scheme" .= ("bearer" :: String),
                            "description" .= ("The session ID of a logged-in user." :: String)
                          ]
                    ]
              ]
        ]
  where
    makeSpec :: [AS.Api.Api] -> AS.AppSpec
    makeSpec apis = basicAppSpec {AS.decls = AS.Decl.makeDecl basicApp : map AS.Decl.makeDecl apis}

    basicApp =
      AS.App.App
        { AS.App.name = "TestApp",
          AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = "^0.0.0"},
          AS.App.title = "Test App",
          AS.App.deployment = Nothing,
          AS.App.db = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = Nothing,
          AS.App.head = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing
        }

    makeApi name route auth =
      AS.Api.Api
        { AS.Api.name = name,
          AS.Api.fn =
            AS.ExtImport.ExtImport
              (AS.ExtImport.ExtImportField name)
              (fromJust $ SP.parseRelFileP "apis")
              Nothing,
          AS.Api.middlewareConfigFn = Nothing,
          AS.Api.entities = Nothing,
          AS.Api.httpRoute = route,
          AS.Api.auth = auth
        }

    operation :: String -> [Value] -> Value
    operation operationId params =
      object $
        ["operationId" .= operationId, "responses" .= defaultResponses]
          ++ ["parameters" .= params | not (null params)]

    defaultResponses = object ["default" .= object ["description" .= ("Response from the API's handler." :: String)]]

    idParam =
      object
        [ "name" .= ("id" :: String),
          "in" .= ("path" :: String),
          "required" .= True,
          "schema" .= object ["type" .= ("string" :: String)]
        ]
