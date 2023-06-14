module Wasp.Generator.WebAppGenerator.ExternalCodeGenerator
  ( extClientCodeGeneratorStrategy,
    extSharedCodeGeneratorStrategy,
    extClientCodeDirInWebAppSrcDir,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ExternalCodeGenerator.Common
  ( ExternalCodeGeneratorStrategy (..),
    GeneratedExternalCodeDir,
    castRelPathFromSrcToGenExtCodeDir,
  )
import Wasp.Generator.ExternalCodeGenerator.Js (resolveJsFileWaspImportsForExtCodeDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util.Path (removePathPrefix)

extClientCodeGeneratorStrategy :: ExternalCodeGeneratorStrategy
extClientCodeGeneratorStrategy = mkExtCodeGeneratorStrategy extClientCodeDirInWebAppSrcDir

extSharedCodeGeneratorStrategy :: ExternalCodeGeneratorStrategy
extSharedCodeGeneratorStrategy = mkExtCodeGeneratorStrategy extSharedCodeDirInWebAppSrcDir

-- | Relative path to the directory where external client code will be generated.
-- Relative to web app src dir.
extClientCodeDirInWebAppSrcDir :: Path' (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir)
extClientCodeDirInWebAppSrcDir = [reldir|ext-src|]

-- | Relative path to the directory where external shared code will be generated.
-- Relative to web app src dir.
extSharedCodeDirInWebAppSrcDir :: Path' (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir)
extSharedCodeDirInWebAppSrcDir = [reldir|shared|]

mkExtCodeGeneratorStrategy :: Path' (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir) -> ExternalCodeGeneratorStrategy
mkExtCodeGeneratorStrategy extCodeDirInWebAppSrcDir =
  ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImportsForExtCodeDir (SP.castRel extCodeDirInWebAppSrcDir),
      _resolveDstFilePath = resolveDstFilePath
    }
  where
    resolveDstFilePath filePath =
      case maybeFilePathInStaticAssetsDir of
        Just filePathInStaticAssetsDir ->
          C.webAppRootDirInProjectRootDir
            </> C.staticAssetsDirInWebAppDir
            </> fromJust (SP.parseRelFile filePathInStaticAssetsDir)
        Nothing ->
          C.webAppRootDirInProjectRootDir
            </> C.webAppSrcDirInWebAppRootDir
            </> extCodeDirInWebAppSrcDir
            </> castRelPathFromSrcToGenExtCodeDir filePath
      where
        maybeFilePathInStaticAssetsDir = removePathPrefix staticAssetsDir (SP.fromRelFile filePath)

    staticAssetsDir = SP.fromRelDir C.staticAssetsDirInWebAppDir
