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
  Path' (Rel SdkTemplatesDir) File' ->
  Path' (Rel SdkRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> relDstPath)
    (sdkTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFd :: Path' (Rel SdkTemplatesDir) File' -> FileDraft
mkTmplFd relSrcPath =
  mkTmplFdWithDstAndData
    relSrcPath
    (SP.castRel relSrcPath)
    Nothing

sdkRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInGeneratedCodeDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk/wasp|]

extSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir GeneratedExternalCodeDir)
extSrcDirInSdkRootDir = [reldir|src|]

makeSdkImportPath :: Path Posix (Rel SdkRootDir) File' -> Path Posix (Rel s) File'
makeSdkImportPath path = (fromJust . parseRelDirP $ sdkPackageName) </> path

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]

serverTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInSdkTemplatesDir = [reldir|server|]

getOperationTypeName :: AS.Operation.Operation -> String
getOperationTypeName operation = toUpperFirst (AS.Operation.getName operation) ++ "_ext"

sdkPackageName :: String
sdkPackageName = "wasp"
