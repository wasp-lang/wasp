module Project.DbTest where

import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import Wasp.Project.Db
  ( DbSystemParseError (..),
    getDbSystemFromPrismaSchema,
  )

spec_DbHelperTest :: Spec
spec_DbHelperTest = do
  describe "getDbSystemFromPrismaSchema" $ do
    it "Correctly extracts PostgreSQL" $ do
      let prismaSchema =
            Util.getPrismaSchema
              [trimming|
                datasource db {
                  provider = "postgresql"
                  url      = env("DATABASE_URL")
                  extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
                }

                generator client {
                  provider = "prisma-client-js"
                  previewFeatures = ["postgresqlExtensions"]
                }
              |]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` Right AS.App.Db.PostgreSQL

    it "Correctly extracts SQLite" $ do
      let prismaSchema =
            Util.getPrismaSchema
              [trimming|
                datasource db {
                  provider = "sqlite"
                  url      = env("DATABASE_URL")
                }

                generator client {
                  provider = "prisma-client-js"
                }
              |]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` Right AS.App.Db.SQLite

    it "Correctly extracts UnsupportedDbSystem" $ do
      let prismaSchema =
            Util.getPrismaSchema
              [trimming|
                datasource db {
                  provider = "mongodb"
                  url      = env("DATABASE_URL")
                }

                generator client {
                  provider = "prisma-client-js"
                }
              |]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` Left (UnsupportedDbSystem "\"mongodb\"")

    it "Correctly extracts MissingDbSystem when provider missing" $ do
      let prismaSchema =
            Util.getPrismaSchema
              [trimming|
                datasource db {
                  url      = env("DATABASE_URL")
                }

                generator client {
                  provider = "prisma-client-js"
                }
              |]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` Left MissingDbSystem

    it "Correctly extracts MissingDbSystem when no datasource" $ do
      let prismaSchema =
            Util.getPrismaSchema
              [trimming|
                generator client {
                  provider = "prisma-client-js"
                }
              |]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` Left MissingDbSystem
