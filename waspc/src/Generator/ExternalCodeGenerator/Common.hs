module Generator.ExternalCodeGenerator.Common
  ( ExternalCodeGeneratorStrategy (..),
    GeneratedExternalCodeDir,
    castRelPathFromSrcToGenExtCodeDir,
    asGenExtFile,
  )
where

import Data.Text (Text)
import ExternalCode (SourceExternalCodeDir)
import Generator.Common (ProjectRootDir)
import qualified Path as P
import StrongPath (Dir, File, Path, Rel)
import qualified StrongPath as SP

-- | Path to the directory where ext code will be generated.
data GeneratedExternalCodeDir

asGenExtFile :: P.Path P.Rel P.File -> Path (Rel GeneratedExternalCodeDir) File
asGenExtFile = SP.fromPathRelFile

castRelPathFromSrcToGenExtCodeDir :: Path (Rel SourceExternalCodeDir) a -> Path (Rel GeneratedExternalCodeDir) a
castRelPathFromSrcToGenExtCodeDir = SP.castRel

data ExternalCodeGeneratorStrategy = ExternalCodeGeneratorStrategy
  { -- | Takes a path where the external code js file will be generated.
    -- Also takes text of the file. Returns text where special @wasp imports have been replaced with
    -- imports that will work.
    _resolveJsFileWaspImports :: Path (Rel GeneratedExternalCodeDir) File -> Text -> Text,
    _extCodeDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir GeneratedExternalCodeDir)
  }
