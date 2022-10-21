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

isTailwindUsed :: AppSpec -> Bool
isTailwindUsed spec =
  configFileExists spec CF.tailwindConfigInWaspDir
    && configFileExists spec CF.postcssConfigInWaspDir

configFileExists :: AppSpec -> Path' (Rel WaspProjectDir) File' -> Bool
configFileExists spec configInWaspDir =
  isJust $
    find
      (\f -> SP.fromRelFile configInWaspDir `isSuffixOf` SP.fromAbsFile (CF._pathInWaspDir f))
      (AS.configFiles spec)

genConfigFiles :: AppSpec -> Generator [FileDraft]
genConfigFiles spec =
  return $ map createFd (AS.configFiles spec)
  where
    createFd configFile = createCopyFileDraft (CF._projectRootDirPath configFile) (CF._pathInWaspDir configFile)
