module Wasp.Generator.SdkGenerator.Root.Common
  ( mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
    genFileCopy,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (File', Path', Rel, castRel, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkTemplatesDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
  )

genFileCopy :: Path' (Rel SdkTemplatesDir) File' -> Generator FileDraft
genFileCopy = return . mkTmplFd

mkTmplFd ::
  Path' (Rel SdkTemplatesDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesDir) File' ->
  Path' (Rel SdkRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData srcFilePath destFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> srcFilePath)
