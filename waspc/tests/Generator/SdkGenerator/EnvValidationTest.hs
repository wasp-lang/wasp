module Generator.SdkGenerator.EnvValidationTest where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Fixtures (systemSPRoot)
import NeatInterpolation (trimming)
import StrongPath (relfile)
import qualified StrongPath as SP
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import Wasp.Generator.FileDraft (FileDraft (FileDraftTemplateFd))
import Wasp.Generator.FileDraft.TemplateFileDraft (TemplateFileDraft (_srcPathInTmplDir, _tmplData))
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Generator.NpmWorkspaces as NW
import Wasp.Generator.SdkGenerator.EnvValidation (genServerEnv)
import Wasp.Generator.Templates (compileAndRenderTemplate)
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Version as WV

-- | These tests render the real "server/env.ts" Mustache template (via the
-- real generator function and the real Mustache engine) to check that the
-- Mailtrap sandbox test-inbox-id validation is wired into the generated
-- code exactly when the Mailtrap provider is enabled. They cover codegen
-- wiring, not the resulting Zod schema's runtime behavior (that was
-- verified manually by running a Wasp app with the Mailtrap provider and
-- confirming the server refuses to start when MAILTRAP_SANDBOX=true and
-- MAILTRAP_TEST_INBOX_ID is missing, and starts fine when it's a valid
-- positive integer).
spec_genServerEnv :: Spec
spec_genServerEnv = do
  describe "genServerEnv" $ do
    it "includes the Mailtrap sandbox test-inbox-id validation when Mailtrap is the email provider" $ do
      content <- renderedEnvTs $ appSpecWithProvider AS.EmailSender.Mailtrap
      T.unpack content `shouldContain` "MAILTRAP_SANDBOX"
      T.unpack content `shouldContain` "MAILTRAP_TEST_INBOX_ID"
      T.unpack content `shouldContain` "superRefine"

    it "does not include the Mailtrap sandbox validation when a different provider is used" $ do
      content <- renderedEnvTs $ appSpecWithProvider AS.EmailSender.SMTP
      T.unpack content `shouldNotContain` "MAILTRAP_SANDBOX"
      T.unpack content `shouldNotContain` "MAILTRAP_TEST_INBOX_ID"
  where
    renderedEnvTs :: AS.AppSpec -> IO T.Text
    renderedEnvTs spec = case runGenerator (genServerEnv spec) of
      (_, Right (FileDraftTemplateFd draft)) ->
        case _tmplData draft of
          Just tmplData -> compileAndRenderTemplate (_srcPathInTmplDir draft) tmplData
          Nothing -> fail "genServerEnv unexpectedly produced a file draft without template data"
      (_, Right _) -> fail "genServerEnv unexpectedly returned a non-template file draft"
      (_, Left err) -> fail $ "genServerEnv failed to generate a file draft: " ++ show err

    appSpecWithProvider :: AS.EmailSender.EmailProvider -> AS.AppSpec
    appSpecWithProvider provider =
      basicAppSpec
        { AS.decls =
            [ AS.Decl.makeDecl "TestApp" $
                basicApp
                  { AS.App.emailSender =
                      Just
                        AS.EmailSender.EmailSender
                          { AS.EmailSender.provider = provider,
                            AS.EmailSender.defaultFrom = Nothing
                          }
                  }
            ]
        }

    basicApp =
      AS.App.App
        { AS.App.wasp =
            AS.Wasp.Wasp
              { AS.Wasp.version = "^" ++ show WV.waspVersion
              },
          AS.App.title = "Test App",
          AS.App.db = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = Nothing,
          AS.App.head = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing
        }

    basicAppSpec =
      AS.AppSpec
        { AS.decls = [],
          AS.prismaSchema = basicPrismaSchema,
          AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
          AS.externalCodeFiles = [],
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

    basicPrismaSchema =
      Util.getPrismaSchema
        [trimming|
          datasource db {
            provider = "postgresql"
            url = env("DATABASE_URL")
          }
        |]
