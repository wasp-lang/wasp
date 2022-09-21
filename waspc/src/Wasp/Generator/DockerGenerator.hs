{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DockerGenerator
  ( genDockerFiles,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.Common (ProjectRootDir, latestMajorNodeVersion)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator (areServerPatchesUsed)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.SemanticVersion as SV

genDockerFiles :: AppSpec -> Generator [FileDraft]
genDockerFiles spec = sequence [genDockerfile spec, genDockerignore spec]

-- TODO: Inject paths to server and db files/dirs, right now they are hardcoded in the templates.
genDockerfile :: AppSpec -> Generator FileDraft
genDockerfile spec = do
  usingServerPatches <- areServerPatchesUsed spec
  return $
    createTemplateFileDraft
      ([relfile|Dockerfile|] :: Path' (Rel ProjectRootDir) File')
      ([relfile|Dockerfile|] :: Path' (Rel TemplatesDir) File')
      ( Just $
          object
            [ "usingPrisma" .= not (null $ AS.getDecls @AS.Entity.Entity spec),
              "nodeMajorVersion" .= show (SV.major latestMajorNodeVersion),
              "usingServerPatches" .= usingServerPatches,
              "extDockerfile" .= fromMaybe "" (AS.dockerfileContents spec)
            ]
      )

genDockerignore :: AppSpec -> Generator FileDraft
genDockerignore _ =
  return $
    createTemplateFileDraft
      ([relfile|.dockerignore|] :: Path' (Rel ProjectRootDir) File')
      ([relfile|dockerignore|] :: Path' (Rel TemplatesDir) File')
      Nothing
