module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Arrow ()
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe, isJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Operations ()
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.Job as AS.Job
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Operation as Operation
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Cli.Common as Common

studio :: Command ()
studio = do
  InWaspProject waspDir <- require

  let generatedProjectDir =
        waspDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  appSpec <- analyze waspDir
  let (appName, app) = ASV.getApp appSpec

  let appInfoJson =
        object
          [ "pages"
              .= map
                ( \(name, _page) ->
                    object
                      [ "name" .= name
                      -- "operations" .= [] -- TODO: Add operations that page uses.
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
            "jobs"
              .= map
                ( \(name, job) ->
                    object
                      [ "name" .= name,
                        "schedule" .= (AS.Job.cron <$> AS.Job.schedule job),
                        "entities"
                          .= ( map
                                 ( \(entityName, _entity) ->
                                     object ["name" .= entityName]
                                 )
                                 $ getJobEntities appSpec job
                             )
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
                          .= ( map
                                 ( \(entityName, _entity) ->
                                     object ["name" .= entityName]
                                 )
                                 $ getOperationEntities appSpec operation
                             )
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
                  "db" .= getDbInfo app
                ]
                -- TODO: Add APIs.
                -- TODO: Add CRUDs.
          ]

  let waspStudioDataJsonFilePath = generatedProjectDir </> [relfile|.wasp-studio-data.json|]
  liftIO $
    BSL.writeFile (SP.toFilePath waspStudioDataJsonFilePath) (encodePretty appInfoJson)
  where
    getOperationEntities :: AS.AppSpec -> AS.Operation.Operation -> [(String, AS.Entity.Entity)]
    getOperationEntities spec operation =
      AS.resolveRef spec <$> fromMaybe [] (Operation.getEntities operation)

    getJobEntities spec job =
      AS.resolveRef spec <$> fromMaybe [] (AS.Job.entities job)

    getDbInfo app = do
      db <- AS.App.db app
      return $
        object
          [ "system" .= (show <$> AS.App.Db.system db)
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
                         [ "google"
                           | isJust $ AS.App.Auth.google methods
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
