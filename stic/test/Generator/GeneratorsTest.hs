{-# LANGUAGE OverloadedStrings #-}
module Generator.GeneratorsTest where

import Test.Tasty.Hspec

import System.FilePath (FilePath, (</>), (<.>))

import Generator.Generators
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.FileDraft.CopyFileDraft
import Wasp

-- TODO(martin): We could define Arbitrary instance for Wasp, define properties over
--   generator functions and then do property testing on them, that would be cool.

spec_Generators :: Spec
spec_Generators = do
    let testApp = (App "TestApp" "Test App")
    let testPage = (Page "TestPage" "/test-page" "<div>Test Page</div>")
    let testWasp = (fromApp testApp) `addPage` testPage

    describe "generateWebApp" $ do
        -- NOTE: This test does not (for now) check that content of files is correct or
        --   that they will successfully be written, it checks only that their
        --   destinations are correct.
        it "Given a simple Wasp, creates file drafts at expected destinations" $ do
            let fileDrafts = generateWebApp testWasp
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
                    , map ("src" </>)
                      [ "logo.png"
                      , "App.css"
                      , "App.js"
                      , "App.test.js"
                      , "index.css"
                      , "index.js"
                      , "reducers.js"
                      , "router.js"
                      , "serviceWorker.js"
                      , (pageName testPage <.> "js")
                      , "store/index.js"
                      , "store/middleware/logger.js"
                      ]
                    ]
            mapM_
                -- NOTE(martin): I added fd to the pair here in order to have it
                --   printed when shouldBe fails, otherwise I could not know which
                --   file draft failed.
                (\dstPath -> (dstPath, existsFdWithDst fileDrafts dstPath)
                    `shouldBe` (dstPath, True))
                expectedFileDraftDstPaths

    describe "generatePage" $ do
        it "Given a simple Wasp, creates template file draft from _Page.js" $ do
            let (FileDraftTemplateFd (TemplateFileDraft _ srcPath _))
                    = generatePage testWasp (head $ getPages testWasp)
            srcPath `shouldBe` "src" </> "_Page.js"


existsFdWithDst :: [FileDraft] -> FilePath -> Bool
existsFdWithDst fds dstPath = any ((== dstPath) . getFileDraftDstPath) fds

getFileDraftDstPath :: FileDraft -> FilePath
getFileDraftDstPath (FileDraftTemplateFd fd) = templateFileDraftDstFilepath fd
getFileDraftDstPath (FileDraftCopyFd fd) = copyFileDraftDstFilepath fd
