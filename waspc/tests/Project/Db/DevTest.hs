module Project.Db.DevTest where

import Fixtures.AppSpec (basicAppSpec)
import NeatInterpolation (trimming)
import System.Environment (setEnv, unsetEnv)
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.Db.Dev (findDevDatabaseUrl)
import qualified Wasp.Project.Db.Dev.Sqlite as DevSqlite

spec_findDevDatabaseUrl :: Spec
spec_findDevDatabaseUrl = do
  describe "findDevDatabaseUrl" $ do
    it "returns the SQLite dev database when the user hasn't configured a database" $ do
      unsetEnv databaseUrlEnvVarName
      findDevDatabaseUrl sqliteAppSpec
        `shouldReturn` Just DevSqlite.defaultDevDbFile

    it ("returns nothing when the user set " <> databaseUrlEnvVarName <> " in .env.server") $ do
      unsetEnv databaseUrlEnvVarName
      findDevDatabaseUrl sqliteAppSpec {AS.devEnvVarsServer = [(databaseUrlEnvVarName, userDbUrl)]}
        `shouldReturn` Nothing

    it ("returns nothing when the user set " <> databaseUrlEnvVarName <> " in the environment") $ do
      setEnv databaseUrlEnvVarName userDbUrl
      result <- findDevDatabaseUrl sqliteAppSpec
      unsetEnv databaseUrlEnvVarName
      result `shouldBe` Nothing
  where
    userDbUrl = "postgresql://userUser:userPass@localhost:9999/userDb"

    sqliteAppSpec = basicAppSpec {AS.prismaSchema = sqlitePrismaSchema}

    sqlitePrismaSchema =
      Util.getPrismaSchema
        [trimming|
          datasource db {
            provider = "sqlite"
            url = env("DATABASE_URL")
          }
        |]
