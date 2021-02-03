module Generator.DockerGenerator
    ( genDockerFiles
    ) where

import           Data.Aeson          (object)
import qualified Path                as P
import           StrongPath          (File, Path, Rel)
import qualified StrongPath          as SP

import           CompileOptions      (CompileOptions)
import           Generator.Common    (ProjectRootDir)
import           Generator.FileDraft (FileDraft, createTemplateFileDraft)
import           Generator.Templates (TemplatesDir)
import           Wasp                (Wasp)

genDockerFiles :: Wasp -> CompileOptions -> [FileDraft]
genDockerFiles wasp _ = concat
    [ [genDockerfile wasp]
    , [genDockerignore wasp]
    ]

-- TODO: Inject paths to server and db files/dirs, right now they are hardcoded in the templates.
genDockerfile :: Wasp -> FileDraft
genDockerfile _ = createTemplateFileDraft
    (SP.fromPathRelFile [P.relfile|Dockerfile|] :: Path (Rel ProjectRootDir) File)
    (SP.fromPathRelFile [P.relfile|Dockerfile|] :: Path (Rel TemplatesDir) File)
    (Just $ object [])

genDockerignore :: Wasp -> FileDraft
genDockerignore _ = createTemplateFileDraft
    (SP.fromPathRelFile [P.relfile|.dockerignore|] :: Path (Rel ProjectRootDir) File)
    (SP.fromPathRelFile [P.relfile|dockerignore|] :: Path (Rel TemplatesDir) File)
    Nothing
