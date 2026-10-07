module Db.RunConfigTest where

import Test.Hspec
import Wasp.AppSpec.App.Db (DbSystem (PostgreSQL, SQLite))
import Wasp.Db.RunConfig
import qualified Wasp.Project.Db.Dev.Postgres as Postgres

spec_connectionUrl :: Spec
spec_connectionUrl = do
  it "has no URL when the local PostgreSQL container was not found" $
    connectionUrl (DbRunConfig PostgreSQL (LocalPostgreSQL Nothing))
      `shouldBe` Nothing

  it "derives the local URL from the discovered port and credentials" $ do
    let db = Postgres.DevDbSpec "volume" "container" "app" "user" "password" 5438
    connectionUrl (DbRunConfig PostgreSQL (LocalPostgreSQL (Just db)))
      `shouldBe` Just "postgresql://user:password@localhost:5438/app"

spec_withConnectionUrl :: Spec
spec_withConnectionUrl = do
  let fallback = DbRunConfig SQLite (SQLiteFile "file:./dev.db")

  it "uses the file URL when neither override is supplied" $
    connectionUrl (resolveDevConnection Nothing Nothing fallback)
      `shouldBe` Just "file:./dev.db"

  it "uses .env.server before the default connection" $
    connectionUrl (resolveDevConnection Nothing (Just "file:./custom.db") fallback)
      `shouldBe` Just "file:./custom.db"

  it "keeps an empty environment override instead of falling back" $ do
    let config = resolveDevConnection (Just "") (Just "file:./custom.db") fallback
    connectionUrl config `shouldBe` Just ""
    provider config `shouldBe` SQLite
    case connection config of
      SuppliedConnection Environment "" -> return ()
      _ -> expectationFailure "Expected the environment URL and its source"

  it "leaves a production connection unconfigured without an explicit URL" $
    connectionUrl (withConnectionUrl CommandOptions Nothing (DbRunConfig PostgreSQL Unconfigured))
      `shouldBe` Nothing
