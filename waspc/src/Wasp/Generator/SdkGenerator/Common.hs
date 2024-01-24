module Wasp.Generator.SdkGenerator.Common where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data SdkRootDir

data SdkTemplatesDir

asTmplFile :: Path' (Rel d) File' -> Path' (Rel SdkTemplatesDir) File'
asTmplFile = SP.castRel

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Path' (Rel SdkRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (sdkRootDirInProjectRootDir </> relDstPath)
    (sdkTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

mkTmplFdWithDst :: Path' (Rel SdkTemplatesDir) File' -> Path' (Rel SdkRootDir) File' -> FileDraft
mkTmplFdWithDst src dst = mkTmplFdWithDstAndData src dst Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData = mkTmplFdWithDstAndData relSrcPath relDstPath (Just tmplData)
  where
    relDstPath = castRel relSrcPath

mkTmplFd :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
mkTmplFd path = mkTmplFdWithDst path (SP.castRel path)

sdkRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInProjectRootDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk|]

extSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir GeneratedExternalCodeDir)
extSrcDirInSdkRootDir = [reldir|ext-src|]

relDirToRelFileP :: Path Posix (Rel d) Dir' -> Path Posix (Rel d) File'
relDirToRelFileP path = fromJust $ SP.parseRelFileP $ removeTrailingSlash $ SP.fromRelDirP path
  where
    removeTrailingSlash = reverse . dropWhile (== '/') . reverse

makeSdkImportPath :: Path Posix (Rel SdkRootDir) File' -> Path Posix (Rel s) File'
makeSdkImportPath path = [reldirP|wasp|] </> path

extCodeDirInSdkRootDir :: Path' (Rel SdkRootDir) Dir'
extCodeDirInSdkRootDir = [reldir|ext-src|]
