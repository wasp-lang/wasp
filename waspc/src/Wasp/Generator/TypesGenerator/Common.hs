module Wasp.Generator.TypesGenerator.Common
  ( TypesRootDir,
    TypesTemplatesDir,
    mkTmplFdWithDstAndData,
    mkTmplFdWithData,
    mkTmplFd,
    typesRootDirInGeneratedCodeDir,
    typesTemplatesDirInTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath
import qualified StrongPath as SP
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data TypesRootDir

data TypesTemplatesDir

mkTmplFd :: Path' (Rel TypesTemplatesDir) File' -> FileDraft
mkTmplFd relSrcPath =
  mkTmplFdWithDstAndData
    relSrcPath
    (SP.castRel relSrcPath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel TypesTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel TypesTemplatesDir) File' ->
  Path' (Rel TypesRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (typesRootDirInGeneratedCodeDir </> relDstPath)
    (typesTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

typesRootDirInGeneratedCodeDir :: Path' (Rel GeneratedAppDir) (Dir TypesRootDir)
typesRootDirInGeneratedCodeDir = [reldir|types|]

typesTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir TypesTemplatesDir)
typesTemplatesDirInTemplatesDir = [reldir|types|]
