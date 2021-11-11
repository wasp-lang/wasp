module Wasp.Generator.WebAppGenerator.ExternalCodeGenerator
  ( extCodeDirInWebAppSrcDir,
    generatorStrategy,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy (..), GeneratedExternalCodeDir)
import Wasp.Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C

-- | Relative path to directory where external code will be generated.
-- Relative to web app src dir.
extCodeDirInWebAppSrcDir :: Path' (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInWebAppSrcDir = [reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy =
  ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInWebAppSrcDir),
      _extCodeDirInProjectRootDir =
        C.webAppRootDirInProjectRootDir
          </> C.webAppSrcDirInWebAppRootDir
          </> extCodeDirInWebAppSrcDir
    }
