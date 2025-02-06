module Wasp.Generator.WebAppGenerator.VitePlugins (genVitePlugins) where

import Data.Aeson (object, (.=))
import StrongPath (Dir, Path', Rel, relfile)
import qualified StrongPath as SP
import Wasp.Generator.Common (WebAppRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromAppComponentDir)

genVitePlugins :: Generator [FileDraft]
genVitePlugins = sequence [genDetectServerImportsPlugin]

genDetectServerImportsPlugin :: Generator FileDraft
genDetectServerImportsPlugin = return $ C.mkTmplFdWithData [relfile|vite/detectServerImports.ts|] tmplData
  where
    tmplData =
      object
        [ "waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir
        ]

    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)
