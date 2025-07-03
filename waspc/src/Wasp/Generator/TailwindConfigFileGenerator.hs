module Wasp.Generator.TailwindConfigFileGenerator
  ( genTailwindConfigFiles,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.ConfigFile as AS.CF
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator)

-- | Generates config file FileDrafts based on the source and destination in ConfigFileRelocator.
genTailwindConfigFiles :: AppSpec -> Generator [FileDraft]
genTailwindConfigFiles spec =
  return $ makeCopyFileDraftForConfigFile <$> AS.tailwindConfigFilesRelocators spec
  where
    makeCopyFileDraftForConfigFile cf =
      createCopyFileDraft
        (AS.CF._pathInProjectRootDir cf)
        (AS.asAbsWaspProjectDirFile spec (AS.CF._pathInWaspProjectDir cf))
