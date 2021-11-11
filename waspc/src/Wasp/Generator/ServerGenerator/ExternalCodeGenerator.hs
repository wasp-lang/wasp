module Wasp.Generator.ServerGenerator.ExternalCodeGenerator
  ( extCodeDirInServerSrcDir,
    generatorStrategy,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy (..), GeneratedExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)
import qualified Wasp.Generator.ServerGenerator.Common as C

-- | Relative path to directory where external code will be generated.
extCodeDirInServerSrcDir :: Path' (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInServerSrcDir = [reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy =
  ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInServerSrcDir),
      _extCodeDirInProjectRootDir =
        C.serverRootDirInProjectRootDir
          </> C.serverSrcDirInServerRootDir
          </> extCodeDirInServerSrcDir
    }
