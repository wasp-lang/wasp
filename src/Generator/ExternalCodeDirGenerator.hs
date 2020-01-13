module Generator.ExternalCodeDirGenerator
       ( generateExternalCodeDir
       , externalCodeDirPathInSrc
       ) where

import System.FilePath ((</>))
import Data.Text (Text)

import CompileOptions (CompileOptions)
import Wasp (Wasp)
import qualified Wasp
import qualified Generator.FileDraft as FD
import qualified Generator.Common as Common


externalCodeDirPathInSrc :: FilePath
externalCodeDirPathInSrc = "ext-src"

generateExternalCodeDir :: CompileOptions -> Wasp -> [FD.FileDraft]
generateExternalCodeDir _ wasp = map generateExternalCodeFile (Wasp.getExternalCodeFiles wasp)

generateExternalCodeFile :: (FilePath, Text) -> FD.FileDraft
generateExternalCodeFile (pathInExtCodeDir, content) = FD.createTextFileDraft dstPath content
  where
    dstPath = Common.srcDirPath </> externalCodeDirPathInSrc </> pathInExtCodeDir



