module Fixtures.AppSpec where

import qualified Data.Map as M
import qualified Data.Set as S
import Fixtures.Paths (systemSPRoot)
import NeatInterpolation (trimming)
import StrongPath (relfile)
import qualified StrongPath as SP
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import qualified Wasp.Generator.NpmWorkspaces as NW
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Psl.Ast.Schema as Psl.Schema

basicAppSpec :: AS.AppSpec
basicAppSpec =
  AS.AppSpec
    { AS.decls = [],
      AS.prismaSchema = basicPrismaSchema,
      AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
      AS.packageJson =
        Npm.PackageJson.PackageJson
          { Npm.PackageJson.name = "testApp",
            Npm.PackageJson.version = Nothing,
            Npm.PackageJson.dependencies = M.empty,
            Npm.PackageJson.devDependencies = M.empty,
            Npm.PackageJson.workspaces = Just $ S.toList NW.requiredWorkspaceGlobs,
            Npm.PackageJson.wasp = Nothing
          },
      AS.buildType = BuildType.Development,
      AS.migrationsDir = Nothing,
      AS.devEnvVarsClient = [],
      AS.devEnvVarsServer = [],
      AS.userDockerfileContents = Nothing,
      AS.devDatabaseUrl = Nothing,
      AS.srcTsConfigPath = [relfile|tsconfig.json|]
    }

basicPrismaSchema :: Psl.Schema.Schema
basicPrismaSchema =
  Util.getPrismaSchema
    [trimming|
      datasource db {
        provider = "postgresql"
        url = env("DATABASE_URL")
      }
      generator client {
        provider = "prisma-client-js"
      }
    |]
