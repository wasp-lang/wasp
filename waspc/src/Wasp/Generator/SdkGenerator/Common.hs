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

-- | SDK root directory in data files templates directory.
data SdkTemplatesRootDir

-- | Directory of some SDK tsconfig project. See 'SdkProject'.
-- In the case of 'RootProject' the directory is the same as 'SdkTemplatesRootDir'.
data SdkTemplatesProjectDir

-- | SDK tsconfig project.
data SdkProject
  = -- | The root SDK tsconfig project which references all other projects.
    RootProject
  | -- | SDK tsconfig project which does not depend on any user code.
    CoreProject
  | -- | SDK tsconfig project which depends on user code.
    UserCoreProject
  | -- | Copy of user's @src@ directory as a SDK tsconfig project.
    ExtSrcProject

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

sdkTemplatesRootDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesRootDir)
sdkTemplatesRootDirInTemplatesDir = [reldir|sdk/wasp|]

sdkTemplatesProjectDirInSdkTemplatesRootDir ::
  SdkProject ->
  Path' (Rel SdkTemplatesRootDir) (Dir SdkTemplatesProjectDir)
sdkTemplatesProjectDirInSdkTemplatesRootDir sdkTmplProject =
  fromJust . parseRelDir $
    case sdkTmplProject of
      RootProject -> "./"
      CoreProject -> "core/"
      UserCoreProject -> "user-core/"
      ExtSrcProject -> toFilePath extSrcDirInSdkRootDir

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
