module Generator.ExternalCodeDirGenerator
       ( generateExternalCodeDir
       , externalCodeDirPathInSrc
       ) where


import System.FilePath ((</>))

import CompileOptions (CompileOptions)
import qualified CompileOptions
import Wasp
import Generator.FileDraft

generateExternalCodeDir :: CompileOptions -> Wasp -> [FileDraft]
generateExternalCodeDir options _ = [createCopyDirDraft dstPath srcPath]
  where
    srcPath = CompileOptions.externalCodeDirPath options
    dstPath = "src" </> externalCodeDirPathInSrc

externalCodeDirPathInSrc :: FilePath
externalCodeDirPathInSrc = "ext-src"
