module Generator.ServerGenerator.ExternalCodeGenerator
    ( extCodeDirInServerSrcDir
    , generatorStrategy
    ) where

import qualified Path as P

import StrongPath (Path, Rel, Dir, (</>))
import qualified StrongPath as SP
import Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy(..), GeneratedExternalCodeDir)
import qualified Generator.ServerGenerator.Common as C
import Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)

-- | Relative path to directory where external code will be generated.
extCodeDirInServerSrcDir :: Path (Rel C.ServerSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInServerSrcDir = SP.fromPathRelDir [P.reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy = ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInServerSrcDir)
    , _extCodeDirInProjectRootDir = C.serverRootDirInProjectRootDir
                                    </> C.serverSrcDirInServerRootDir
                                    </> extCodeDirInServerSrcDir
    }
