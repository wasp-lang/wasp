module Wasp.Generator.StudioGenerator
  ( genStudio,
    studioDataFileInGeneratedAppDir,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import StrongPath (File', Path', Rel, relfile)
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
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import Wasp.Generator.Monad (Generator)

-- | Wasp Studio draws the app as a graph from this file. It is rewritten on
-- every compile, so the open Studio panels update with the app.
studioDataFileInGeneratedAppDir :: Path' (Rel GeneratedAppDir) File'
studioDataFileInGeneratedAppDir = [relfile|.wasp-studio-data.json|]

genStudio :: AS.AppSpec -> Generator [FileDraft]
genStudio spec =
  return
    [ createTextFileDraft studioDataFileInGeneratedAppDir
        $ TL.toStrict
        $ TLE.decodeUtf8
        $ encodePretty
        $ getStudioData spec
    ]

getStudioData :: AS.AppSpec -> Value
getStudioData appSpec =
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
            "auth" .= getAuthInfo appSpec,
            "db" .= getDbInfo appSpec
          ]
          -- TODO: Add CRUDs.
    ]
  where
    app = ASV.getApp appSpec

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

    getAuthInfo spec = do
      auth <- AS.App.auth $ ASV.getApp spec
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
