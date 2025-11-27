module Wasp.Generator.SdkGenerator.Vite (genVite) where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Vite.VitePlugin (genVitePlugins)

genVite :: AppSpec -> Generator [FileDraft]
genVite spec = genVitePlugins spec
