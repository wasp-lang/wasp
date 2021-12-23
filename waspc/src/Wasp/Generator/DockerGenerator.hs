{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.DockerGenerator
  ( genDockerFiles,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Entity as AS.Entity
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

genDockerFiles :: AppSpec -> [FileDraft]
genDockerFiles spec = genDockerfile spec : [genDockerignore spec]

-- TODO: Inject paths to server and db files/dirs, right now they are hardcoded in the templates.
genDockerfile :: AppSpec -> FileDraft
genDockerfile spec =
  createTemplateFileDraft
    ([relfile|Dockerfile|] :: Path' (Rel ProjectRootDir) File')
    ([relfile|Dockerfile|] :: Path' (Rel TemplatesDir) File')
    ( Just $
        object
          [ "usingPrisma" .= not (null $ AS.getDecls @AS.Entity.Entity spec)
          ]
    )

genDockerignore :: AppSpec -> FileDraft
genDockerignore _ =
  createTemplateFileDraft
    ([relfile|.dockerignore|] :: Path' (Rel ProjectRootDir) File')
    ([relfile|dockerignore|] :: Path' (Rel TemplatesDir) File')
    Nothing
