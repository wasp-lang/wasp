module Wasp.Generator.TailwindConfigFile
  ( isTailwindUsed,
    tailwindConfigRelocationMap,
  )
where

import Data.List (find)
import Data.Map (fromList)
import Data.Maybe (isJust)
import StrongPath (File', Path', Rel, castRel, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.ConfigFile as CF
import Wasp.ConfigFile (ConfigFileRelocationMap)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import Wasp.Project.Common (WaspProjectDir)

tailwindConfigFile :: Path' (Rel WaspProjectDir) File'
tailwindConfigFile = [relfile|tailwind.config.js|]

postcssConfigFile :: Path' (Rel WaspProjectDir) File'
postcssConfigFile = [relfile|postcss.config.js|]

asProjectRootDirConfigFile :: Path' (Rel WaspProjectDir) File' -> Path' (Rel ProjectRootDir) File'
asProjectRootDirConfigFile = (webAppRootDirInProjectRootDir </>) . castRel

-- | Helper that determines if Tailwind used. For our purposes, we allow
-- developers to opt-in by creating both a tailwind config file and
-- postcss config file in their wasp project dir.
isTailwindUsed :: AppSpec -> Bool
isTailwindUsed spec =
  doesConfigFileExist tailwindConfigFile
    && doesConfigFileExist postcssConfigFile
  where
    doesConfigFileExist :: Path' (Rel WaspProjectDir) File' -> Bool
    doesConfigFileExist file =
      isJust $ find ((==) file . CF._pathInWaspProjectDir) $ AS.tailwindConfigFilesRelocators spec

-- Establishes the mapping of which Tailwind configs to copy and where from/to.
tailwindConfigRelocationMap :: ConfigFileRelocationMap
tailwindConfigRelocationMap =
  fromList
    [ (tailwindConfigFile, asProjectRootDirConfigFile tailwindConfigFile),
      (postcssConfigFile, asProjectRootDirConfigFile postcssConfigFile)
    ]
