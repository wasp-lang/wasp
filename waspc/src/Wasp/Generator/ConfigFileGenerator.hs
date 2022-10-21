module Wasp.Generator.ConfigFileGenerator
  ( isTailwindUsed,
    genConfigFiles,
  )
where

import Data.List (find, isSuffixOf)
import Data.Maybe (isJust)
import StrongPath (File', Path', Rel)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Common (WaspProjectDir)
import qualified Wasp.ConfigFile as CF
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator)

-- | Helper that determines if Tailwind used. For our purposes, we allow
-- developers to opt-in by creating both a tailwind config file and
-- postcss config file in their wasp project dir.
isTailwindUsed :: AppSpec -> Bool
isTailwindUsed spec =
  isConfigFile spec CF.tailwindConfigFile
    && isConfigFile spec CF.postcssConfigFile

-- | Internal helper for determining if some wasp project file is a tracked config file in appspec.
isConfigFile :: AppSpec -> Path' (Rel WaspProjectDir) File' -> Bool
isConfigFile spec file =
  isJust $
    find
      (\f -> SP.fromRelFile file `isSuffixOf` SP.fromAbsFile (CF._pathInWaspDir f))
      (AS.configFiles spec)

-- | Generates config file FileDrafts based on the source and destination in ConfigFileRelocator.
genConfigFiles :: AppSpec -> Generator [FileDraft]
genConfigFiles spec =
  return $ map createFd (AS.configFiles spec)
  where
    createFd :: CF.ConfigFileRelocator -> FileDraft
    createFd configFile = createCopyFileDraft (CF._projectRootDirPath configFile) (CF._pathInWaspDir configFile)
