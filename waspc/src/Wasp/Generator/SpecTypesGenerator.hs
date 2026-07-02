module Wasp.Generator.SpecTypesGenerator
  ( genSpecTypes,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, castRel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec, getEntities)
import Wasp.Generator.Common (GeneratedAppDir, makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.NodePackageFFI (InstallablePackage (..), getInstallablePackageName)

genSpecTypes :: AppSpec -> Generator [FileDraft]
genSpecTypes spec =
  if null entities
    then return []
    else
      return
        [ mkTmplFdWithData
            [relfile|register.ts|]
            ( object
                [ "entities" .= entities,
                  "waspSpecPackageName" .= waspSpecPackageName
                ]
            )
        ]
  where
    entities = map (makeJsonWithEntityData . fst) $ getEntities spec

    waspSpecPackageName :: String
    waspSpecPackageName = getInstallablePackageName WaspSpecPackage

data SpecTypesRootDir

data SpecTypesTemplatesDir

mkTmplFdWithData ::
  Path' (Rel SpecTypesTemplatesDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData =
  mkTmplFdWithDstAndData
    relSrcPath
    (castRel relSrcPath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SpecTypesTemplatesDir) File' ->
  Path' (Rel SpecTypesRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (specTypesRootDirInGeneratedCodeDir </> relDstPath)
    (specTypesTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

specTypesRootDirInGeneratedCodeDir :: Path' (Rel GeneratedAppDir) (Dir SpecTypesRootDir)
specTypesRootDirInGeneratedCodeDir = [reldir|types/spec|]

specTypesTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SpecTypesTemplatesDir)
specTypesTemplatesDirInTemplatesDir = [reldir|types/spec|]
