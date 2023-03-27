module Wasp.Cli.Command.Db.Seed
  ( seed,
  )
where

import Control.Monad (when)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isNothing)
import StrongPath ((</>))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.ExtImport as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.Generator.DbGenerator.Operations (dbSeed)
import qualified Wasp.Message as Msg

seed :: Maybe String -> Command ()
seed maybeSeedName = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  appSpec <- analyze waspProjectDir
  when (isNothing $ getDbSeedFn appSpec) $
    (E.throwError . CommandError "No seed specified") $
      "You haven't specified any database seeding functions, so there is nothing to run!\n"
        <> "To do so, set app.db.seed in your Wasp config."

  cliSendMessageC $ Msg.Start "Seeding the database..."

  liftIO (dbSeed genProjectDir maybeSeedName) >>= \case
    Left errorMsg -> cliSendMessageC $ Msg.Failure "Database seeding failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database seeded successfully!"

getDbSeedFn :: AS.AppSpec -> Maybe [AS.ExtImport]
getDbSeedFn spec = AS.Db.seeds =<< AS.App.db (snd $ ASV.getApp spec)
