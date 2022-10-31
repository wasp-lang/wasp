module Wasp.Generator.ServerGenerator.ExternalCodeGenerator
  ( extServerCodeGeneratorStrategy,
    extServerCodeDirInServerSrcDir,
    extSharedCodeGeneratorStrategy,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy (..), GeneratedExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)
import qualified Wasp.Generator.ServerGenerator.Common as C

extServerCodeGeneratorStrategy :: ExternalCodeGeneratorStrategy
extServerCodeGeneratorStrategy = mkExtCodeGeneratorStrategy extServerCodeDirInServerSrcDir

extSharedCodeGeneratorStrategy :: ExternalCodeGeneratorStrategy
extSharedCodeGeneratorStrategy = mkExtCodeGeneratorStrategy extSharedCodeDirInServerSrcDir

-- | Relative path to the directory where external server code will be generated.
-- Relative to the server src dir.
extServerCodeDirInServerSrcDir :: Path' (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir)
extServerCodeDirInServerSrcDir = [reldir|ext-src|]

-- | Relative path to the directory where external shared code will be generated.
-- Relative to the server src dir.
extSharedCodeDirInServerSrcDir :: Path' (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir)
extSharedCodeDirInServerSrcDir = [reldir|shared|]

mkExtCodeGeneratorStrategy :: Path' (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir) -> ExternalCodeGeneratorStrategy
mkExtCodeGeneratorStrategy extCodeDirInServerSrcDir =
  ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInServerSrcDir),
      _extCodeDirInProjectRootDir =
        C.serverRootDirInProjectRootDir
          </> C.serverSrcDirInServerRootDir
          </> extCodeDirInServerSrcDir
    }
