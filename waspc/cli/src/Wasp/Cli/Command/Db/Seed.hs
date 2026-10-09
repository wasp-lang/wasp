module Wasp.Cli.Command.Db.Seed
  ( seed,
  )
where

import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.RunConfigs (makeDefaultDevRunConfigs)
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import Wasp.Generator.DbGenerator.Operations (dbSeed)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

seed :: Arguments -> Command ()
seed = withArguments seedArgsParser $ \SeedArgs {_seedName = maybeUserProvidedSeedName} -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  appSpec <- analyze waspProjectDir
  let (_, serverRunConfig) = makeDefaultDevRunConfigs appSpec

  nameOfSeedToRun <- obtainNameOfExistingSeedToRun maybeUserProvidedSeedName appSpec

  cliSendMessageC $ Msg.Start $ "Running database seed " <> nameOfSeedToRun <> "..."

  liftIO (dbSeed serverRunConfig genProjectDir nameOfSeedToRun) >>= \case
    Left errorMsg -> E.throwError $ CommandError "Database seeding failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database seeded successfully!"

newtype SeedArgs = SeedArgs
  { _seedName :: Maybe String
  }

seedArgsParser :: ArgsParser SeedArgs
seedArgsParser =
  ArgsParser "wasp db seed" $
    SeedArgs
      <$> Opt.optional
        ( Opt.strArgument $
            Opt.metavar "SEED_NAME"
              <> Opt.help "Name of the seed to run. If omitted and more than one seed is defined, you will be asked to pick one"
        )

obtainNameOfExistingSeedToRun :: Maybe String -> AS.AppSpec -> Command String
obtainNameOfExistingSeedToRun maybeUserProvidedSeedName spec = do
  seedNames <- getSeedNames <$> getSeedsFromAppSpecOrThrowIfNone
  case maybeUserProvidedSeedName of
    Just name -> parseUserProvidedSeedName name seedNames
    Nothing -> case seedNames of
      seedName :| [] -> return seedName
      _seedNames -> liftIO $ Interactive.askToChoose "Choose a seed to run" seedNames
  where
    parseUserProvidedSeedName :: String -> NE.NonEmpty String -> Command String
    parseUserProvidedSeedName userProvidedSeedName seedNames =
      if userProvidedSeedName `elem` seedNames
        then return userProvidedSeedName
        else
          (E.throwError . CommandError "Invalid seed name") $
            "There is no seed with the name "
              <> userProvidedSeedName
              <> "."
              <> ("\nValid seed names are: " <> intercalate ", " (NE.toList seedNames) <> ".")

    getSeedsFromAppSpecOrThrowIfNone :: Command (NE.NonEmpty AS.ExtImport.ExtImport)
    getSeedsFromAppSpecOrThrowIfNone = case getDbSeeds spec of
      Just seeds@(_ : _) -> return $ NE.fromList seeds
      _noSeeds ->
        (E.throwError . CommandError "No seeds defined") $
          "You haven't defined any database seeding functions, so there is nothing to run!\n"
            <> "To do so, define seeding functions via app.db.seeds in your Wasp spec."

    getSeedNames :: (Functor f) => f AS.ExtImport.ExtImport -> f String
    getSeedNames seeds = AS.ExtImport.importIdentifier <$> seeds

getDbSeeds :: AS.AppSpec -> Maybe [AS.ExtImport.ExtImport]
getDbSeeds spec = AS.Db.seeds =<< AS.App.db (ASV.getApp spec)
