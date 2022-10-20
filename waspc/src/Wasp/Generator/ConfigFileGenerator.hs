module Wasp.Generator.ConfigFileGenerator
  ( isTailwindUsed,
    genWebAppConfigFiles,
    genServerConfigFiles,
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
import Wasp.Generator.ServerGenerator.Common (serverRootDirInProjectRootDir)
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)

isTailwindUsed :: AppSpec -> Bool
isTailwindUsed spec = configExists "tailwind.config.js" && configExists "postcss.config.js"
  where
    configFilePath configFile = SP.toFilePath . CF._pathInWaspDir $ configFile
    configExists filename = isJust $ find (\f -> filename `isSuffixOf` configFilePath f) (AS.configFiles spec)

genServerConfigFiles :: AppSpec -> Generator [FileDraft]
genServerConfigFiles spec = do
  return $ map createFd serverConfigFilePaths
  where
    serverConfigFiles = filter (isJust . CF.serverDstPath) (AS.configFiles spec)
    serverConfigFilePaths = map CF._pathInWaspDir serverConfigFiles
    createFd filePath =
      createCopyFileDraft
        (serverRootDirInProjectRootDir SP.</> SP.basename filePath)
        filePath

genWebAppConfigFiles :: AppSpec -> Generator [FileDraft]
genWebAppConfigFiles spec = do
  return $ map createFd webAppConfigFilePaths
  where
    webAppConfigFiles = filter (isJust . CF.webAppDstPath) (AS.configFiles spec)
    webAppConfigFilePaths = map CF._pathInWaspDir webAppConfigFiles
    createFd filePath =
      createCopyFileDraft
        (webAppRootDirInProjectRootDir SP.</> SP.basename filePath)
        filePath
