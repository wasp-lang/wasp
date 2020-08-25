module Generator.WebAppGenerator.ExternalCodeGenerator
    ( extCodeDirInWebAppSrcDir
    , generatorStrategy
    ) where

import qualified Path as P

import StrongPath (Path, Rel, Dir, (</>))
import qualified StrongPath as SP
import Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy(..), GeneratedExternalCodeDir)
import qualified Generator.WebAppGenerator.Common as C
import Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)

-- | Relative path to directory where external code will be generated.
-- Relative to web app src dir.
extCodeDirInWebAppSrcDir :: Path (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInWebAppSrcDir = SP.fromPathRelDir [P.reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy = ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInWebAppSrcDir)
    , _extCodeDirInProjectRootDir = C.webAppRootDirInProjectRootDir
                                    </> C.webAppSrcDirInWebAppRootDir
                                    </> extCodeDirInWebAppSrcDir
    }
