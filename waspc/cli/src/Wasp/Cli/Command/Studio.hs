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
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Operations ()
import qualified System.Directory as Dir
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.Job as AS.Job
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as Operation
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Port (resolvePort)
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import Wasp.Cli.Util.PortArgument (portParser)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
import qualified Wasp.Project.Studio

newtype StudioArgs = StudioArgs
  { port :: Maybe PortNumber
  }

studioArgsParser :: ArgsParser StudioArgs
studioArgsParser =
  ArgsParser "wasp studio" $
    StudioArgs
      <$> Opt.optional (portParser "port" ("Port to run Wasp Studio on (default: " ++ show defaultStudioPort ++ ")"))

studio :: Arguments -> Command ()
studio = withArguments studioArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require

  appSpec <- analyze waspDir
  let app = ASV.getApp appSpec

  let appInfoJson =
        object
          [ "pages"
              .= map
                ( \page ->
                    object
                      [ "name" .= page.name,
                        "authRequired" .= AS.Page.authRequired page
                        -- "operations" .= [] -- TODO: Add operations that page uses. Not easy.
                      ]
                )
                (AS.getPages appSpec),
            "routes"
              .= map
                ( \route ->
                    object
                      [ "name" .= route.name,
                        "path" .= AS.Route.path route,
                        "toPage"
                          .= object
                            [ "name" .= AS.Page.name (AS.resolveRef appSpec route.to)
                            ]
                      ]
                )
                (AS.getRoutes appSpec),
            "apis"
              .= map
                ( \api ->
                    object
                      [ "name" .= api.name,
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
                ( \job ->
                    object
                      [ "name" .= job.name,
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
                          _op@(QueryOp _) -> "query" :: String
                          _op@(ActionOp _) -> "action",
                        "name" .= Operation.getName operation,
                        "entities"
                          .= getLinkedEntitiesData appSpec (Operation.getEntities operation),
                        "auth" .= Operation.getAuth operation
                      ]
                )
                (AS.getOperations appSpec),
            "entities"
              .= map
                ( \entity ->
                    object
                      [ "name" .= AS.Entity.getName entity
                      ]
                )
                (AS.getEntities appSpec),
            "app"
              .= object
                [ "name" .= (app.name :: String),
                  "auth" .= getAuthInfo appSpec app,
                  "db" .= getDbInfo appSpec
                ]
                -- TODO: Add CRUDs.
          ]

  let generatedProjectDir =
        waspDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  let waspStudioDataJsonFilePath = generatedProjectDir </> [relfile|.wasp-studio-data.json|]
  liftIO $ do
    Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent waspStudioDataJsonFilePath
    BSL.writeFile (SP.fromAbsFile waspStudioDataJsonFilePath) (encodePretty appInfoJson)

  studioPort <-
    resolvePort
      args.port
      defaultStudioPort
      []
      "Choose a different port with --port, or free up this one."
      "Free at least one of those ports by exiting the program listening on it, or choose the port yourself with --port."

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: http://localhost:" ++ show studioPort,
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected."
      ]

  result <- liftIO $ do
    Wasp.Project.Studio.startStudio (SP.toFilePath waspStudioDataJsonFilePath) studioPort

  either (throwError . CommandError "Studio command failed") return result
  where
    getLinkedEntitiesData spec entityRefs =
      map
        ( \entity ->
            object ["name" .= AS.Entity.getName entity]
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
                [ "name" .= AS.Entity.getName (AS.resolveRef spec auth.userEntity)
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

-- | The port Wasp Studio runs on when it isn't given one.
defaultStudioPort :: PortNumber
defaultStudioPort = 4000
