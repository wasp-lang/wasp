module Generator.WebAppGenerator.ExternalCodeGenerator
  ( extCodeDirInWebAppSrcDir,
    generatorStrategy,
  )
where

import Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy (..), GeneratedExternalCodeDir)
import Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)
import qualified Generator.WebAppGenerator.Common as C
import qualified Path as P
import StrongPath (Dir, Path, Rel, (</>))
import qualified StrongPath as SP

-- | Relative path to directory where external code will be generated.
-- Relative to web app src dir.
extCodeDirInWebAppSrcDir :: Path (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInWebAppSrcDir = SP.fromPathRelDir [P.reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy =
  ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInWebAppSrcDir),
      _extCodeDirInProjectRootDir =
        C.webAppRootDirInProjectRootDir
          </> C.webAppSrcDirInWebAppRootDir
          </> extCodeDirInWebAppSrcDir
    }
