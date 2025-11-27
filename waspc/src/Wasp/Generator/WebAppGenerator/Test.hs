module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path', relfile)
import qualified StrongPath as SP
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp args projectDir = do
  -- Run vitest from the project root with the root vite.config.ts
  runNodeCommandAsJob projectDir "npx" (vitestCommand ++ args) J.WebApp
  where
    vitestCommand = ["vitest", "--config", SP.fromRelFile viteConfigPath]
    -- User's vite.config.ts in the project root
    viteConfigPath = [relfile|vite.config.ts|]
