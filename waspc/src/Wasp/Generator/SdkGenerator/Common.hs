module Wasp.Generator.SdkGenerator.Common where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
import qualified StrongPath as SP
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Util (toUpperFirst)

data SdkRootDir

data SdkTemplatesDir

data ClientTemplatesDir

data ServerTemplatesDir

mkTmplFdWithDstAndData ::
  Path' (Rel SdkRootDir) File' ->
  Path' (Rel SdkTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relDstPath relSrcPath tmplData =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> relDstPath)
    (sdkTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

mkTmplFdWithDst ::
  Path' (Rel SdkTemplatesDir) File' ->
  Path' (Rel SdkRootDir) File' ->
  FileDraft
mkTmplFdWithDst src dst = mkTmplFdWithDstAndData dst src Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData = mkTmplFdWithDstAndData relDstPath relSrcPath (Just tmplData)
  where
    relDstPath = castRel relSrcPath

mkTmplFd :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
mkTmplFd path = mkTmplFdWithDst path (SP.castRel path)

sdkRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInGeneratedCodeDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk/wasp|]

extSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir GeneratedExternalCodeDir)
extSrcDirInSdkRootDir = [reldir|src|]

relDirToRelFileP :: Path Posix (Rel d) Dir' -> Path Posix (Rel d) File'
relDirToRelFileP path = fromJust $ SP.parseRelFileP $ removeTrailingSlash $ SP.fromRelDirP path
  where
    removeTrailingSlash = reverse . dropWhile (== '/') . reverse

makeSdkImportPath :: Path Posix (Rel SdkRootDir) File' -> Path Posix (Rel s) File'
makeSdkImportPath path = [reldirP|wasp|] </> path

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]

serverTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInSdkTemplatesDir = [reldir|server|]

getOperationTypeName :: AS.Operation.Operation -> String
getOperationTypeName operation = toUpperFirst (AS.Operation.getName operation) ++ "_ext"
