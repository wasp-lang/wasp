module Wasp.Generator.ConfigFileGenerator
  ( isTailwindUsed,
    genConfigFiles,
  )
where

import Data.List (find, isSuffixOf)
import Data.Maybe (isJust)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ConfigFiles as CF
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator)

isTailwindUsed :: AppSpec -> Bool
isTailwindUsed spec = configExists "tailwind.config.js" && configExists "postcss.config.js"
  where
    configFilePath configFileName = SP.toFilePath . CF._pathInWaspDir $ configFileName
    configExists filename = isJust $ find (\f -> filename `isSuffixOf` configFilePath f) (AS.configFiles spec)

genConfigFiles :: AppSpec -> Generator [FileDraft]
genConfigFiles spec = do
  return $ map createFd (AS.configFiles spec)
  where
    createFd configFile =
      createCopyFileDraft
        (CF._projectRootDirPath configFile)
        (CF._pathInWaspDir configFile)
