module Generator.DockerGenerator
  ( genDockerFiles,
  )
where

import CompileOptions (CompileOptions)
import Data.Aeson (object, (.=))
import Generator.Common (ProjectRootDir)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Generator.Templates (TemplatesDir)
import StrongPath (File', Path', Rel, relfile)
import Wasp (Wasp)
import qualified Wasp

genDockerFiles :: Wasp -> CompileOptions -> [FileDraft]
genDockerFiles wasp _ =
  concat
    [ [genDockerfile wasp],
      [genDockerignore wasp]
    ]

-- TODO: Inject paths to server and db files/dirs, right now they are hardcoded in the templates.
genDockerfile :: Wasp -> FileDraft
genDockerfile wasp =
  createTemplateFileDraft
    ([relfile|Dockerfile|] :: Path' (Rel ProjectRootDir) File')
    ([relfile|Dockerfile|] :: Path' (Rel TemplatesDir) File')
    ( Just $
        object
          [ "usingPrisma" .= not (null $ Wasp.getPSLEntities wasp)
          ]
    )

genDockerignore :: Wasp -> FileDraft
genDockerignore _ =
  createTemplateFileDraft
    ([relfile|.dockerignore|] :: Path' (Rel ProjectRootDir) File')
    ([relfile|dockerignore|] :: Path' (Rel TemplatesDir) File')
    Nothing
