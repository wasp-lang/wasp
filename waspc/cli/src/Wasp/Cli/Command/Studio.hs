module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Arrow ()
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import StrongPath.Operations ()
import Text.Pretty.Simple (pPrint)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.AppSpec.Operation (Operation (..))
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Operation as Operation
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Message as Msg

studio :: Command ()
studio = do
  InWaspProject waspDir <- require

  appSpec <- analyze waspDir
  let (appName, app) = ASV.getApp appSpec

  let appInfoJson =
        Aeson.object
          [ "pages"
              Aeson..= map
                ( \(name, page) ->
                    Aeson.object
                      [ "name" Aeson..= name
                      -- "operations" Aeson..= [] -- TODO: Add operations that page uses.
                      ]
                )
                (AS.getPages appSpec),
            "operations"
              Aeson..= map
                ( \operation ->
                    Aeson.object
                      [ "type" Aeson..= case operation of
                          _op@(QueryOp _ _) -> "query" :: String
                          _op@(ActionOp _ _) -> "action",
                        "name" Aeson..= Operation.getName operation,
                        "entities"
                          Aeson..= ( map
                                       ( \(entityName, _entity) ->
                                           Aeson.object ["name" Aeson..= entityName]
                                       )
                                       $ getOperationEntities appSpec operation
                                   )
                      ]
                )
                (AS.getOperations appSpec),
            "entities"
              Aeson..= map
                ( \(name, _entity) ->
                    Aeson.object
                      [ "name" Aeson..= name
                      ]
                )
                (AS.getEntities appSpec),
            "app"
              Aeson..= Aeson.object
                [ "name" Aeson..= (appName :: String)
                ]
          ]

  liftIO $ pPrint appInfoJson
  where
    getOperationEntities :: AS.AppSpec -> AS.Operation.Operation -> [(String, AS.Entity.Entity)]
    getOperationEntities spec operation =
      AS.resolveRef spec <$> fromMaybe [] (Operation.getEntities operation)
