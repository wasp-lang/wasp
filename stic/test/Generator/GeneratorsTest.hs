{-# LANGUAGE OverloadedStrings #-}
module Generator.GeneratorsTest where

import qualified Test.Tasty
import Test.Tasty.Hspec

import System.FilePath (FilePath, (</>))
import Data.Aeson as Aeson (object, (.=))

import Generator.Generators
import Generator.FileDraft
import Generator.FileDraft.TemplateFileDraft
import Generator.FileDraft.CopyFileDraft
import Wasp


spec_Generators :: Spec
spec_Generators = do
    describe "generateWebApp" $ do
        -- NOTE: This test does not (for now) check that content of files is correct or
        --   that they will successfully be written, it checks only that their
        --   destinations are correct.
        it "Given a simple Wasp, creates file drafts at expected destinations." $ do
            let fileDrafts = generateWebApp simpleWasp
            let expectedFileDrafts = concat $
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
                      , "serviceWorker.js"
                      ]
                    ]
            mapM_
                -- NOTE(martin): I added fd to the pair here in order to have it
                --   printed when shouldBe fails, otherwise I could not know which
                --   file draft failed.
                (\fd -> (fd, existsFdWithDst fileDrafts fd) `shouldBe` (fd, True))
                expectedFileDrafts
              where
                (appName, appTitle) = ("TestApp", "Test App")

                simpleWasp :: Wasp
                simpleWasp = fromApp $ App appName appTitle

                existsFdWithDst :: [FileDraft] -> FilePath -> Bool
                existsFdWithDst fds dstPath =
                    length (filter ((== dstPath). getFileDraftDstPath) fds) == 1


getFileDraftDstPath :: FileDraft -> FilePath
getFileDraftDstPath (FileDraftTemplateFd fd) = templateFileDraftDstFilepath fd
getFileDraftDstPath (FileDraftCopyFd fd) = copyFileDraftDstFilepath fd
