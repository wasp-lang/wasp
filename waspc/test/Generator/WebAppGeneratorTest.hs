module Generator.WebAppGeneratorTest where

import Fixtures (systemSPRoot)
import qualified StrongPath as SP
import System.FilePath ((</>))
import Test.Tasty.Hspec
import qualified Wasp.CompileOptions as CompileOptions
import Wasp.Generator.FileDraft
import qualified Wasp.Generator.FileDraft.CopyDirFileDraft as CopyDirFD
import qualified Wasp.Generator.FileDraft.CopyFileDraft as CopyFD
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Wasp.Generator.FileDraft.TextFileDraft as TextFD
import Wasp.Generator.WebAppGenerator
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import Wasp.Wasp

-- TODO(martin): We could define Arbitrary instance for Wasp, define properties over
--   generator functions and then do property testing on them, that would be cool.

spec_WebAppGenerator :: Spec
spec_WebAppGenerator = do
  let testApp = App "TestApp" "Test App" Nothing
  let testWasp = fromApp testApp
  let testCompileOptions =
        CompileOptions.CompileOptions
          { CompileOptions.externalCodeDirPath = systemSPRoot SP.</> [SP.reldir|test/src|],
            CompileOptions.isBuild = False
          }

  describe "generateWebApp" $ do
    -- NOTE: This test does not (for now) check that content of files is correct or
    --   that they will successfully be written, it checks only that their
    --   destinations are correct.
    it "Given a simple Wasp, creates file drafts at expected destinations" $ do
      let fileDrafts = generateWebApp testWasp testCompileOptions
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
                      "index.css",
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
