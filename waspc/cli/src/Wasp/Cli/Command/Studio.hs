module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Arrow ()
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe, isJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Operations ()
import qualified System.Directory as Dir
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.Job as AS.Job
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as Operation
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import qualified Wasp.Project.Studio

studio :: Command ()
studio = do
  InWaspProject waspDir <- require

  appSpec <- analyze waspDir
  let (appName, app) = ASV.getApp appSpec

  let appInfoJson =
        object
          [ "pages"
              .= map
                ( \(name, page) ->
                    object
                      [ "name" .= name,
                        "authRequired" .= AS.Page.authRequired page
                        -- "operations" .= [] -- TODO: Add operations that page uses. Not easy.
                      ]
                )
                (AS.getPages appSpec),
            "routes"
              .= map
                ( \(name, route) ->
                    object
                      [ "name" .= name,
                        "path" .= AS.Route.path route,
                        "toPage"
                          .= object
                            [ "name" .= fst (AS.resolveRef appSpec $ AS.Route.to route)
                            ]
                      ]
                )
                (AS.getRoutes appSpec),
            "apis"
              .= map
                ( \(name, api) ->
                    object
                      [ "name" .= name,
                        "httpRoute"
                          .= let (method, path) = AS.Api.httpRoute api
                              in object
                                   [ "method" .= show method,
                                     "path" .= path
                                   ],
                        "auth" .= AS.Api.auth api,
                        "entities" .= getLinkedEntitiesData appSpec (AS.Api.entities api)
                      ]
                )
                (AS.getApis appSpec),
            "jobs"
              .= map
                ( \(name, job) ->
                    object
                      [ "name" .= name,
                        "schedule" .= (AS.Job.cron <$> AS.Job.schedule job),
                        "entities" .= getLinkedEntitiesData appSpec (AS.Job.entities job)
                      ]
                )
                (AS.getJobs appSpec),
            "operations"
              .= map
                ( \operation ->
                    object
                      [ "type" .= case operation of
                          _op@(QueryOp _ _) -> "query" :: String
                          _op@(ActionOp _ _) -> "action",
                        "name" .= Operation.getName operation,
                        "entities"
                          .= getLinkedEntitiesData appSpec (Operation.getEntities operation),
                        "auth" .= Operation.getAuth operation
                      ]
                )
                (AS.getOperations appSpec),
            "entities"
              .= map
                ( \(name, _entity) ->
                    object
                      [ "name" .= name
                      ]
                )
                (AS.getEntities appSpec),
            "app"
              .= object
                [ "name" .= (appName :: String),
                  "auth" .= getAuthInfo appSpec app,
                  "db" .= getDbInfo appSpec
                ]
                -- TODO: Add CRUDs.
          ]

  let generatedProjectDir =
        waspDir
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir

  let waspStudioDataJsonFilePath = generatedProjectDir </> [relfile|.wasp-studio-data.json|]
  liftIO $ do
    Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent waspStudioDataJsonFilePath
    BSL.writeFile (SP.fromAbsFile waspStudioDataJsonFilePath) (encodePretty appInfoJson)

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: http://localhost:4000",
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected."
      ]

  result <- liftIO $ do
    Wasp.Project.Studio.startStudio $ SP.toFilePath waspStudioDataJsonFilePath

  either (throwError . CommandError "Studio command failed") return result
  where
    getLinkedEntitiesData spec entityRefs =
      map
        ( \(entityName, _entity) ->
            object ["name" .= entityName]
        )
        $ resolveEntities spec entityRefs

    resolveEntities spec entityRefs =
      AS.resolveRef spec <$> fromMaybe [] entityRefs

    getDbInfo spec =
      object
        [ "system" .= show (ASV.getValidDbSystem spec)
        ]

    getAuthInfo spec app = do
      auth <- AS.App.auth app
      return $
        object
          [ "userEntity"
              .= object
                [ "name" .= fst (AS.resolveRef spec $ AS.App.Auth.userEntity auth)
                ],
            "methods"
              .= let methods = AS.App.Auth.methods auth
                  in -- TODO: Make this type safe, so it gives compile time error/warning if
                     --   new field is added to AuthMethods and we haven't covered it here.
                     --   Best to use TH here to generate this object from AuthMethods?
                     concat
                       [ [ "usernameAndPassword"
                           | isJust $ AS.App.Auth.usernameAndPassword methods
                         ],
                         [ "slack"
                           | isJust $ AS.App.Auth.slack methods
                         ],
                         [ "discord"
                           | isJust $ AS.App.Auth.discord methods
                         ],
                         [ "google"
                           | isJust $ AS.App.Auth.google methods
                         ],
                         [ "keycloak"
                           | isJust $ AS.App.Auth.keycloak methods
                         ],
                         [ "gitHub"
                           | isJust $ AS.App.Auth.gitHub methods
                         ],
                         [ "email"
                           | isJust $ AS.App.Auth.email methods
                         ]
                       ] ::
                       [String]
          ]
