module Generator.WebAppGeneratorTest where

import qualified Data.Map as M
import Fixtures
import StrongPath (relfile)
import qualified StrongPath as SP
import System.FilePath ((</>))
import Test.Tasty.Hspec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.FileDraft
import qualified Wasp.Generator.FileDraft.CopyAndModifyTextFileDraft as CMTextFD
import qualified Wasp.Generator.FileDraft.CopyDirFileDraft as CopyDirFD
import qualified Wasp.Generator.FileDraft.CopyFileDraft as CopyFD
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Wasp.Generator.FileDraft.TextFileDraft as TextFD
import Wasp.Generator.Monad (runGenerator)
import Wasp.Generator.WebAppGenerator
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Version as WV

-- TODO(martin): We could maybe define Arbitrary instance for AppSpec, define properties
-- over generator functions and then do property testing on them, that would be cool.

spec_WebAppGenerator :: Spec
spec_WebAppGenerator = do
  let testAppSpec =
        AS.AppSpec
          { AS.decls =
              [ AS.Decl.makeDecl
                  "TestApp"
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
              ],
            AS.prismaSchema = Psl.Schema.Schema [],
            AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
            AS.externalCodeFiles = [],
            AS.externalPublicFiles = [],
            AS.packageJson =
              Npm.PackageJson.PackageJson
                { Npm.PackageJson.name = "testApp",
                  Npm.PackageJson.dependencies = M.empty,
                  Npm.PackageJson.devDependencies = M.empty
                },
            AS.isBuild = False,
            AS.migrationsDir = Nothing,
            AS.devEnvVarsServer = [],
            AS.devEnvVarsClient = [],
            AS.userDockerfileContents = Nothing,
            AS.tailwindConfigFilesRelocators = [],
            AS.devDatabaseUrl = Nothing,
            AS.customViteConfigPath = Nothing,
            AS.srcTsConfigPath = [relfile|tsconfig.json|],
            AS.srcTsConfig =
              T.TsConfig
                { T.compilerOptions =
                    T.CompilerOptions
                      { T._module = Just "esnext",
                        T.composite = Just True,
                        T.target = Just "esnext",
                        T.moduleResolution = Just "bundler",
                        T.jsx = Just "preserve",
                        T.strict = Just True,
                        T.esModuleInterop = Just True,
                        T.isolatedModules = Just True,
                        T.moduleDetection = Just "force",
                        T.lib = Just ["dom", "dom.iterable", "esnext"],
                        T.skipLibCheck = Just True,
                        T.allowJs = Just True,
                        T.outDir = Just ".wasp/out/user"
                      },
                  T.include = Just ["src"]
                }
          }

  describe "genWebApp" $ do
    -- NOTE: This test does not (for now) check that content of files is correct or
    --   that they will successfully be written, it checks only that their
    --   destinations are correct.
    it "Given a simple AppSpec, creates file drafts at expected destinations" $ do
      let (_, Right fileDrafts) = runGenerator $ genWebApp testAppSpec
      let expectedFileDraftDstPaths =
            map (SP.toFilePath Common.webAppRootDirInProjectRootDir </>) $
              concat
                [ [ "README.md",
                    "package.json",
                    ".gitignore",
                    "index.html"
                  ],
                  ["public" </> "manifest.json"],
                  map
                    (SP.toFilePath Common.webAppSrcDirInWebAppRootDir </>)
                    [ "logo.png",
                      "index.tsx",
                      "router.tsx"
                    ]
                ]

      mapM_
        -- NOTE(martin): I added fd to the pair here in order to have it
        --   printed when shouldBe fails, otherwise I could not know which
        --   file draft failed.
        ( \dstPath ->
            (dstPath, existsFdWithDst fileDrafts dstPath)
              `shouldBe` (dstPath, True)
        )
        expectedFileDraftDstPaths

existsFdWithDst :: [FileDraft] -> FilePath -> Bool
existsFdWithDst fds dstPath = any ((== dstPath) . getFileDraftDstPath) fds

-- TODO(martin): This should really become part of the Writeable typeclass,
--   since it is smth we want to do for all file drafts.
getFileDraftDstPath :: FileDraft -> FilePath
getFileDraftDstPath (FileDraftTemplateFd fd) = SP.toFilePath $ TmplFD._dstPath fd
getFileDraftDstPath (FileDraftCopyFd fd) = SP.toFilePath $ CopyFD._dstPath fd
getFileDraftDstPath (FileDraftCopyDirFd fd) = SP.toFilePath $ CopyDirFD._dstPath fd
getFileDraftDstPath (FileDraftTextFd fd) = SP.toFilePath $ TextFD._dstPath fd
getFileDraftDstPath (FileDraftCopyAndModifyTextFd fd) = SP.toFilePath $ CMTextFD._dstPath fd
