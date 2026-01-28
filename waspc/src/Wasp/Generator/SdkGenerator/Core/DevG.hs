module Wasp.Generator.SdkGenerator.Core.DevG
  ( genDev,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir, Path', Rel, fromRelDir, relfile)
import Wasp.Generator.Common (WebAppRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFdWithData)
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromAppComponentDir)

genDev :: Generator [FileDraft]
genDev =
  return [mkTmplFdWithData [relfile|dev/index.ts|] tmplData]
  where
    tmplData = object ["waspProjectDirFromWebAppDir" .= fromRelDir waspProjectDirFromWebAppDir]
    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)
