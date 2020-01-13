module Generator.GeneratorsTest where

import Test.Tasty.Hspec

import System.FilePath (FilePath, (</>), (<.>))

import Util
import qualified CompileOptions
import Generator.Generators
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.FileDraft.CopyFileDraft
import Generator.FileDraft.TextFileDraft
import qualified Generator.Common as Common
import Wasp

-- TODO(martin): We could define Arbitrary instance for Wasp, define properties over
--   generator functions and then do property testing on them, that would be cool.

spec_Generators :: Spec
spec_Generators = do
    let testApp = (App "TestApp" "Test App")
    let testPage = (Page "TestPage" "/test-page" "<div>Test Page</div>" Nothing)
    let testEntity = (Entity "TestEntity" [EntityField "testField" EftString])
    let testWasp = (fromApp testApp) `addPage` testPage `addEntity` testEntity
    let testCompileOptions = CompileOptions.CompileOptions
            { CompileOptions.externalCodeDirPath = "test/src"
            }

    describe "generateWebApp" $ do
        -- NOTE: This test does not (for now) check that content of files is correct or
        --   that they will successfully be written, it checks only that their
        --   destinations are correct.
        it "Given a simple Wasp, creates file drafts at expected destinations" $ do
            let fileDrafts = generateWebApp testWasp testCompileOptions
            let testEntityDstDirInSrc
                    = "entities" </> (Util.camelToKebabCase (entityName testEntity))
            let expectedFileDraftDstPaths = concat $
                    [ [ "README.md"
                      , "package.json"
                      , ".gitignore"
                      ]
                    , map ("public" </>)
                      [ "favicon.ico"
                      , "index.html"
                      , "manifest.json"
                      ]
                    , map (Common.srcDirPath </>)
                      [ "logo.png"
                      , "index.css"
                      , "index.js"
                      , "reducers.js"
                      , "router.js"
                      , "serviceWorker.js"
                      , (pageName testPage <.> "js")
                      , "store/index.js"
                      , "store/middleware/logger.js"
                      , testEntityDstDirInSrc </> "actions.js"
                      , testEntityDstDirInSrc </> "actionTypes.js"
                      , testEntityDstDirInSrc </> "state.js"
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
getFileDraftDstPath (FileDraftTemplateFd fd) = templateFileDraftDstFilepath fd
getFileDraftDstPath (FileDraftCopyFd fd) = copyFileDraftDstFilepath fd
getFileDraftDstPath (FileDraftTextFd fd) = textFileDraftDstFilepath fd
