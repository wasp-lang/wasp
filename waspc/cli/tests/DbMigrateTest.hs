module DbMigrateTest where

import Data.Either (isLeft)
import qualified Options.Applicative as Opt
import Test.Hspec
import Wasp.Cli.Command.Db.Migrate (migrateArgsParser)
import Wasp.Cli.Command.Start.ArgumentsParser (StartDbArgs (..), startDbArgsParser)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)

spec_databaseOptions :: Spec
spec_databaseOptions = do
  it "parses database and migration options together" $ do
    parseOptions ["--name", "new model", "--db-image", "postgis:18", "--create-only", "--db-port", "5544"]
      `shouldBe` Right (StartDbArgs (Just 5544) (Just "postgis:18") Nothing, MigrateArgs (Just "new model") True)
    parseOptions ["--db-volume-mount-path=/var/lib/postgresql/data"]
      `shouldBe` Right (StartDbArgs Nothing Nothing (Just "/var/lib/postgresql/data"), defaultMigrateArgs)
    parseOptions [] `shouldBe` Right (StartDbArgs Nothing Nothing Nothing, defaultMigrateArgs)
  it "rejects missing values and unknown options" $ do
    mapM_
      (\args -> isLeft (parseOptions args) `shouldBe` True)
      [["--db-port"], ["--db-port", "0"], ["--db-port", "65536"], ["--db-port", "abc"], ["--db-image"], ["--name", "--db-image", "postgres:18"], ["--unknown"]]
  where
    parseOptions = parse ((,) <$> startDbArgsParser <*> migrateArgsParser)

parse :: Opt.Parser a -> [String] -> Either String a
parse parser args =
  case Opt.execParserPure Opt.defaultPrefs (Opt.info parser Opt.fullDesc) args of
    Opt.Success value -> Right value
    Opt.Failure failure -> Left $ fst $ Opt.renderFailure failure "wasp db migrate-dev"
    Opt.CompletionInvoked _ -> Left "Unexpected completion"
