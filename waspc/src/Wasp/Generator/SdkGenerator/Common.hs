module Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkSrcDir,
    SdkTemplatesDir,
    ClientTemplatesDir,
    ServerTemplatesDir,
    ViteDir,
    VitePluginsDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
    genFileCopy,
    sdkRootDirInGeneratedAppDir,
    sdkTemplatesDirInTemplatesDir,
    sdkSrcDirInSdkRootDir,
    clientTemplatesDirInSdkTemplatesDir,
    serverTemplatesDirInSdkTemplatesDir,
    getOperationTypeName,
    viteDirInSdkTemplatesDir,
    vitePluginsDirInSdkTemplatesDir,
    sdkPackageName,
    libsRootDirFromSdkDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath
import qualified StrongPath as SP
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Generator.Common (GeneratedAppComponentDir, GeneratedAppComponentSrcDir, GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Generator.WaspLibs.Common as WaspLibsC
import Wasp.Util (toUpperFirst)
import Wasp.Util.StrongPath (invertRelDir)

data SdkRootDir

instance GeneratedAppComponentDir SdkRootDir

data SdkSrcDir

instance GeneratedAppComponentSrcDir SdkSrcDir

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

sdkSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkSrcDir)
sdkSrcDirInSdkRootDir = [reldir|src|]

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]

serverTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInSdkTemplatesDir = [reldir|server|]

getOperationTypeName :: AS.Operation.Operation -> String
getOperationTypeName operation = toUpperFirst (AS.Operation.getName operation) ++ "Resolved"

viteDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ViteDir)
viteDirInSdkTemplatesDir = [reldir|client/vite|]

vitePluginsDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir VitePluginsDir)
vitePluginsDirInSdkTemplatesDir = viteDirInSdkTemplatesDir </> [reldir|plugins|]

sdkPackageName :: String
sdkPackageName = "wasp"

libsRootDirFromSdkDir :: Path' (Rel SdkRootDir) (Dir WaspLibsC.LibsRootDir)
libsRootDirFromSdkDir = invertRelDir sdkRootDirInGeneratedAppDir </> WaspLibsC.libsRootDirInGeneratedAppDir
