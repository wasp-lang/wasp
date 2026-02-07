module Wasp.Generator.SdkGenerator.UserCore.Common
  ( SdkTemplatesUserCoreDir,
    SdkUserCoreDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
    genFileCopy,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, castRel, reldir, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkTemplatesDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
  )

-- | Directory of the SDK user core tsconfig project in generated code.
-- It contains all logic dependent on the user's project.
data SdkUserCoreDir

-- | Directory of the SDK user core tsconfig project in templates.
-- It contains all logic dependent on the user's project.
data SdkTemplatesUserCoreDir

genFileCopy :: Path' (Rel SdkTemplatesUserCoreDir) File' -> Generator FileDraft
genFileCopy = return . mkTmplFd

mkTmplFd ::
  Path' (Rel SdkTemplatesUserCoreDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesUserCoreDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesUserCoreDir) File' ->
  Path' (Rel SdkUserCoreDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData srcFilePath destFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkUserCoreDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> sdkTemplatesUserCoreDirInSdkTemplatesDir </> srcFilePath)

sdkTemplatesUserCoreDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir SdkTemplatesUserCoreDir)
sdkTemplatesUserCoreDirInSdkTemplatesDir = [reldir|user-core|]

sdkUserCoreDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkUserCoreDir)
sdkUserCoreDirInSdkRootDir = [reldir|user-core|]
