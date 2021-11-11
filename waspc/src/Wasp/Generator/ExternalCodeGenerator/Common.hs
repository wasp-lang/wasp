module Wasp.Generator.ExternalCodeGenerator.Common
  ( ExternalCodeGeneratorStrategy (..),
    GeneratedExternalCodeDir,
    castRelPathFromSrcToGenExtCodeDir,
    asGenExtFile,
  )
where

import Data.Text (Text)
import StrongPath (Dir, File', Path', Rel)
import qualified StrongPath as SP
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.Generator.Common (ProjectRootDir)

-- | Path to the directory where ext code will be generated.
data GeneratedExternalCodeDir

asGenExtFile :: Path' (Rel d) File' -> Path' (Rel GeneratedExternalCodeDir) File'
asGenExtFile = SP.castRel

castRelPathFromSrcToGenExtCodeDir :: Path' (Rel SourceExternalCodeDir) a -> Path' (Rel GeneratedExternalCodeDir) a
castRelPathFromSrcToGenExtCodeDir = SP.castRel

data ExternalCodeGeneratorStrategy = ExternalCodeGeneratorStrategy
  { -- | Takes a path where the external code js file will be generated.
    -- Also takes text of the file. Returns text where special @wasp imports have been replaced with
    -- imports that will work.
    _resolveJsFileWaspImports :: Path' (Rel GeneratedExternalCodeDir) File' -> Text -> Text,
    _extCodeDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir GeneratedExternalCodeDir)
  }
