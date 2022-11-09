module Generator.WebAppGeneratorTest where

import Fixtures (systemSPRoot)
import qualified StrongPath as SP
import System.FilePath ((</>))
import Test.Tasty.Hspec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import Wasp.Generator.FileDraft
import qualified Wasp.Generator.FileDraft.CopyDirFileDraft as CopyDirFD
import qualified Wasp.Generator.FileDraft.CopyFileDraft as CopyFD
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Wasp.Generator.FileDraft.TextFileDraft as TextFD
import Wasp.Generator.Monad (runGenerator)
import Wasp.Generator.WebAppGenerator
import qualified Wasp.Generator.WebAppGenerator.Common as Common
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
                      AS.App.dependencies = Nothing,
                      AS.App.head = Nothing
                    }
              ],
            AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
            AS.externalCodeDirPath = systemSPRoot SP.</> [SP.reldir|test/src|],
            AS.externalCodeFiles = [],
            AS.isBuild = False,
            AS.migrationsDir = Nothing,
            AS.dotEnvServerFile = Nothing,
            AS.dotEnvClientFile = Nothing,
            AS.userDockerfileContents = Nothing,
            AS.configFiles = []
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
                    ".gitignore"
                  ],
                  map
                    ("public" </>)
                    [ "favicon.ico",
                      "index.html",
                      "manifest.json"
                    ],
                  map
                    (SP.toFilePath Common.webAppSrcDirInWebAppRootDir </>)
                    [ "logo.png",
                      "index.js",
                      "router.js",
                      "serviceWorker.js"
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
