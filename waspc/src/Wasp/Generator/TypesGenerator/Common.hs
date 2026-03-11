module Wasp.Generator.TypesGenerator.Common where

import qualified Data.Aeson as Aeson
import StrongPath
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data TypesRootDir

data TypesTemplatesDir

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

mkTmplFdWithData ::
  Path' (Rel TypesTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFd :: Path' (Rel TypesTemplatesDir) File' -> FileDraft
mkTmplFd relSrcPath =
  mkTmplFdWithDstAndData
    relSrcPath
    (SP.castRel relSrcPath)
    Nothing

typesRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir TypesRootDir)
typesRootDirInGeneratedCodeDir = [reldir|types|]

typesTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir TypesTemplatesDir)
typesTemplatesDirInTemplatesDir = [reldir|types|]
