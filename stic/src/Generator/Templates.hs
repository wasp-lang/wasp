module Generator.Templates
       ( getTemplatesDirAbsPath
       , getTemplateFileAbsPath
       ) where

import qualified Paths_stic
import System.FilePath ((</>))

-- | Returns absolute path of templates root directory.
--   NOTE(Martin): Here I set it to react-app, which might be one lvl too deep
--     and will require some changes in the future, but did not want to
--     overengineer for now.
getTemplatesDirAbsPath :: IO FilePath
getTemplatesDirAbsPath = do
    dataDirPath <- Paths_stic.getDataDir
    return $ dataDirPath </> "Generator/templates/react-app"

getTemplateFileAbsPath :: FilePath -> IO FilePath
getTemplateFileAbsPath relFilepath =
    Paths_stic.getDataFileName ("Generator/templates/react-app" </> relFilepath)
