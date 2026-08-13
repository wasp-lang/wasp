module Wasp.Generator.SdkGenerator.Common where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath
import qualified StrongPath as SP
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Generator.WaspLibs.Common as WaspLibsC
import Wasp.Util (toUpperFirst)
import Wasp.Util.StrongPath (invertRelDir)

data SdkRootDir

data SdkTemplatesDir

data ClientTemplatesDir

data ServerTemplatesDir

data ViteDir

data VitePluginsDir

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Path' (Rel SdkRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (sdkRootDirInGeneratedAppDir </> relDstPath)
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

genFileCopy :: Path' (Rel SdkTemplatesDir) File' -> Generator FileDraft
genFileCopy = return . mkTmplFd

sdkRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir SdkRootDir)
sdkRootDirInGeneratedAppDir = [reldir|sdk/wasp|]

sdkTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkTemplatesDirInTemplatesDir = [reldir|sdk/wasp|]

makeSdkImportPath :: Path Posix (Rel SdkRootDir) File' -> Path Posix (Rel s) File'
makeSdkImportPath path = (fromJust . parseRelDirP $ sdkPackageName) </> path

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]

serverTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInSdkTemplatesDir = [reldir|server|]

getGenericOperationDefinitionTypeName :: AS.Operation.Operation -> String
getGenericOperationDefinitionTypeName = toUpperFirst . AS.Operation.getName

getRegisteredOperationTypeName :: AS.Operation.Operation -> String
getRegisteredOperationTypeName operation = "Registered" ++ getGenericOperationDefinitionTypeName operation

clientViteDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ViteDir)
clientViteDirInSdkTemplatesDir = [reldir|client/vite|]

clientVitePluginsDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir VitePluginsDir)
clientVitePluginsDirInSdkTemplatesDir = clientViteDirInSdkTemplatesDir </> [reldir|plugins|]

serverViteDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ViteDir)
serverViteDirInSdkTemplatesDir = [reldir|server/vite|]

serverVitePluginsDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir VitePluginsDir)
serverVitePluginsDirInSdkTemplatesDir = serverViteDirInSdkTemplatesDir </> [reldir|plugins|]

sdkPackageName :: String
sdkPackageName = "wasp"

libsRootDirFromSdkDir :: Path' (Rel SdkRootDir) (Dir WaspLibsC.LibsRootDir)
libsRootDirFromSdkDir = invertRelDir sdkRootDirInGeneratedAppDir </> WaspLibsC.libsRootDirInGeneratedAppDir
