module Wasp.Generator.SdkGenerator.Common where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Project.Common (generatedCodeDirInDotWaspDir)
import Wasp.Util (toUpperFirst)

-- | SDK root directory in a generated Wasp project.
data SdkRootDir

-- | SDK root directory from any generated project directory (e.g. `out` or `build`).
sdkRootDirInGeneratedProjectDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInGeneratedProjectDir = [reldir|sdk/wasp/|]

-- | Currently, all generated projects point to the same SDK root (`.wasp/out/sdk/wasp`).
-- To understand what's going on here, read this issue:
-- https://github.com/wasp-lang/wasp/issues/1769
sdkRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInProjectRootDir =
  [reldir|../|]
    </> generatedCodeDirInDotWaspDir
    </> sdkRootDirInGeneratedProjectDir

-- | External `src` directory refers to the user's `src` directory.
extSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir GeneratedExternalCodeDir)
extSrcDirInSdkRootDir = [reldir|src|]

-- | SDK root directory in data files templates directory.
data SdkTemplatesRootDir

sdkTemplatesRootDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesRootDir)
sdkTemplatesRootDirInTemplatesDir = [reldir|sdk/wasp|]

-- | Directory of some SDK tsconfig project. See 'SdkProject'.
-- In the case of 'RootSdkProject' the directory is the same as 'SdkTemplatesRootDir'.
data SdkTemplatesProjectDir

-- | SDK tsconfig project.
data SdkProject = SdkRootProject | SdkCoreProject | SdkUserCoreProject | SdkExtSrcProject

sdkTemplatesProjectDirInSdkTemplatesRootDir ::
  SdkProject ->
  Path' (Rel SdkTemplatesRootDir) (Dir SdkTemplatesProjectDir)
sdkTemplatesProjectDirInSdkTemplatesRootDir sdkTmplProject =
  fromJust . parseRelDir $
    case sdkTmplProject of
      SdkRootProject -> "./"
      SdkCoreProject -> "core/"
      SdkUserCoreProject -> "user-core/"
      SdkExtSrcProject -> toFilePath extSrcDirInSdkRootDir

makeSdkProjectTmplFdWithDestAndData ::
  Path' (Rel SdkTemplatesProjectDir) File' ->
  SdkProject ->
  Path' (Rel SdkTemplatesProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
makeSdkProjectTmplFdWithDestAndData destFile sdkTmplProject tmplFile tmplData =
  createTemplateFileDraft
    (castDir sdkRootDirInProjectRootDir </> sdkTemplatesProjectDirInSdkTemplatesRootDir sdkTmplProject </> destFile)
    (sdkTemplatesRootDirInTemplatesDir </> sdkTemplatesProjectDirInSdkTemplatesRootDir sdkTmplProject </> tmplFile)
    tmplData

makeSdkProjectTmplFdWithData ::
  SdkProject ->
  Path' (Rel SdkTemplatesProjectDir) File' ->
  Aeson.Value ->
  FileDraft
makeSdkProjectTmplFdWithData sdkTmplProject tmplFile tmplData =
  makeSdkProjectTmplFdWithDestAndData (castRel tmplFile) sdkTmplProject tmplFile (Just tmplData)

makeSdkProjectTmplFd ::
  SdkProject ->
  Path' (Rel SdkTemplatesProjectDir) File' ->
  FileDraft
makeSdkProjectTmplFd sdkTmplProject tmplFile =
  makeSdkProjectTmplFdWithDestAndData (castRel tmplFile) sdkTmplProject tmplFile Nothing

relDirToRelFileP :: Path Posix (Rel d) Dir' -> Path Posix (Rel d) File'
relDirToRelFileP path = fromJust $ parseRelFileP $ removeTrailingSlash $ fromRelDirP path
  where
    removeTrailingSlash = reverse . dropWhile (== '/') . reverse

makeSdkImportPath :: Path Posix (Rel SdkRootDir) File' -> Path Posix (Rel s) File'
makeSdkImportPath path = [reldirP|wasp|] </> path

getOperationTypeName :: AS.Operation.Operation -> String
getOperationTypeName operation = toUpperFirst (AS.Operation.getName operation) ++ "_ext"
