module Generator.ServerGenerator.ExternalCodeGenerator
  ( extCodeDirInServerSrcDir,
    generatorStrategy,
  )
where

import Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy (..), GeneratedExternalCodeDir)
import Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)
import qualified Generator.ServerGenerator.Common as C
import qualified Path as P
import StrongPath (Dir, Path, Rel, (</>))
import qualified StrongPath as SP

-- | Relative path to directory where external code will be generated.
extCodeDirInServerSrcDir :: Path (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInServerSrcDir = SP.fromPathRelDir [P.reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy =
  ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInServerSrcDir),
      _extCodeDirInProjectRootDir =
        C.serverRootDirInProjectRootDir
          </> C.serverSrcDirInServerRootDir
          </> extCodeDirInServerSrcDir
    }
