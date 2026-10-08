module DbMigrateTest where

import qualified Options.Applicative as Opt
import Test.Hspec
import Wasp.Cli.Command.Db.Migrate (migrateArgsParser)
import Wasp.Cli.Command.Db.StartOptions (DbStartOptions (..), dbStartOptionsParser)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)

spec_databaseOptions :: Spec
spec_databaseOptions = do
  it "parses database and migration options together" $ do
    parseOptions ["--name", "new model", "--db-image", "postgis:18", "--create-only", "--db-port", "5544"]
      `shouldBe` Right (DbStartOptions (Just 5544) (Just "postgis:18") Nothing, MigrateArgs (Just "new model") True)
    parseOptions [] `shouldBe` Right (DbStartOptions Nothing Nothing Nothing, defaultMigrateArgs)
  where
    parseOptions = parse ((,) <$> dbStartOptionsParser <*> migrateArgsParser)

parse :: Opt.Parser a -> [String] -> Either String a
parse parser args =
  case Opt.execParserPure Opt.defaultPrefs (Opt.info parser Opt.fullDesc) args of
    Opt.Success value -> Right value
    Opt.Failure failure -> Left $ fst $ Opt.renderFailure failure "wasp db migrate-dev"
    Opt.CompletionInvoked _ -> Left "Unexpected completion"
