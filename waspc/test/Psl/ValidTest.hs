module Psl.ValidTest where

import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.Psl.Valid as PslV
import qualified Wasp.Valid as Valid

spec_PrismaSchemaValid :: Spec
spec_PrismaSchemaValid = do
  describe "validatePrismaSchema" $ do
    describe "datasource validation" $ do
      it "should validate that some datasource exists" $
        let prismaSchema =
              Util.getPrismaSchema
                [trimming|
                  generator client {
                    provider = "prisma-client-js"
                  }
                |]
         in PslV.validatePrismaSchema prismaSchema
              `shouldBe` [Valid.GenericValidationError "Prisma schema must have exactly one datasource defined."]

      it "should validate that only one datasource exists" $
        let prismaSchema =
              Util.getPrismaSchema
                [trimming|
                  datasource db {}
                  datasource db {}
                  generator client {
                    provider = "prisma-client-js"
                  }
                |]
         in PslV.validatePrismaSchema prismaSchema
              `shouldBe` [Valid.GenericValidationError "Prisma schema must have exactly one datasource defined."]

    describe "generators validation" $ do
      it "should validate that there is at least one generator" $
        let prismaSchema =
              Util.getPrismaSchema
                [trimming|
                  datasource db {
                    provider = "postgresql"
                    url      = env("DATABASE_URL")
                  }
                |]
         in PslV.validatePrismaSchema prismaSchema
              `shouldBe` [Valid.GenericValidationError "Prisma schema should have at least one generator defined."]

      it "should validate that there is at least one client generator" $
        let prismaSchema =
              Util.getPrismaSchema
                [trimming|
                  datasource db {
                    provider = "postgresql"
                    url      = env("DATABASE_URL")
                  }
                  generator bla {
                    provider = "bla"
                  }
                |]
         in PslV.validatePrismaSchema prismaSchema
              `shouldBe` [Valid.GenericValidationError "Prisma schema should have at least one generator with the provider set to \"prisma-client-js\"."]
      -- No error, missing db system and unsupported db system error cases
      describe "db system validation" $ do
        it "should not return any error when db system is supported" $
          let prismaSchema =
                Util.getPrismaSchema
                  [trimming|
                    datasource db {
                      provider = "postgresql"
                      url      = env("DATABASE_URL")
                    }

                    generator client {
                      provider = "prisma-client-js"
                      previewFeatures = ["postgresqlExtensions"]
                    }
                  |]
           in PslV.validatePrismaSchema prismaSchema `shouldBe` []

        it "should return an error when db system is missing" $
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
           in PslV.validatePrismaSchema prismaSchema
                `shouldBe` [Valid.GenericValidationError "You need to specify the \"provider\" field in the \"datasource\" block in your Prisma schema."]

        it "should return an error when db system is unsupported" $
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
           in PslV.validatePrismaSchema prismaSchema
                `shouldBe` [Valid.GenericValidationError "Wasp doesn't support the database provider \"mongodb\" specified in the schema.prisma file."]
