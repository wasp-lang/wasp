module Generator.WebAppGeneratorTest where

import Test.Tasty.Hspec

import System.FilePath ((</>))
import qualified Path as P

import qualified StrongPath as SP
import qualified CompileOptions
import Generator.WebAppGenerator
import Generator.FileDraft
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Generator.FileDraft.CopyFileDraft as CopyFD
import qualified Generator.FileDraft.TextFileDraft as TextFD
import qualified Generator.WebAppGenerator.Common as Common
import Wasp
import Fixtures (systemPathRoot)

-- TODO(martin): We could define Arbitrary instance for Wasp, define properties over
--   generator functions and then do property testing on them, that would be cool.

spec_WebAppGenerator :: Spec
spec_WebAppGenerator = do
    let testApp = (App "TestApp" "Test App" Nothing)
    let testWasp = (fromApp testApp)
    let testCompileOptions = CompileOptions.CompileOptions
            { CompileOptions.externalCodeDirPath = SP.fromPathAbsDir $ systemPathRoot P.</> [P.reldir|test/src|]
            , CompileOptions.waspIgnoreFilePath = SP.fromPathAbsFile $ systemPathRoot P.</> [P.relfile|test/.waspignore|]
            , CompileOptions.isBuild = False
            }

    describe "generateWebApp" $ do
        -- NOTE: This test does not (for now) check that content of files is correct or
        --   that they will successfully be written, it checks only that their
        --   destinations are correct.
        it "Given a simple Wasp, creates file drafts at expected destinations" $ do
            let fileDrafts = generateWebApp testWasp testCompileOptions
            let expectedFileDraftDstPaths = map ((SP.toFilePath Common.webAppRootDirInProjectRootDir) </>) $ concat $
                    [ [ "README.md"
                      , "package.json"
                      , ".gitignore"
                      ]
                    , map ("public" </>)
                      [ "favicon.ico"
                      , "index.html"
                      , "manifest.json"
                      ]
                    , map ((SP.toFilePath Common.webAppSrcDirInWebAppRootDir) </>)
                      [ "logo.png"
                      , "index.css"
                      , "index.js"
                      , "router.js"
                      , "serviceWorker.js"
                      ]
                    ]

            mapM_
                -- NOTE(martin): I added fd to the pair here in order to have it
                --   printed when shouldBe fails, otherwise I could not know which
                --   file draft failed.
                (\dstPath -> (dstPath, existsFdWithDst fileDrafts dstPath)
                    `shouldBe` (dstPath, True))
                expectedFileDraftDstPaths


existsFdWithDst :: [FileDraft] -> FilePath -> Bool
existsFdWithDst fds dstPath = any ((== dstPath) . getFileDraftDstPath) fds

-- TODO(martin): This should really become part of the Writeable typeclass,
--   since it is smth we want to do for all file drafts.
getFileDraftDstPath :: FileDraft -> FilePath
getFileDraftDstPath (FileDraftTemplateFd fd) = SP.toFilePath $ TmplFD._dstPath fd
getFileDraftDstPath (FileDraftCopyFd fd) = SP.toFilePath $ CopyFD._dstPath fd
getFileDraftDstPath (FileDraftTextFd fd) = SP.toFilePath $ TextFD._dstPath fd
