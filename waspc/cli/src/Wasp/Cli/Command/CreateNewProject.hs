module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (..))
import Path.IO (copyDirRecur, doesDirExist)
import StrongPath (Abs, Dir, Path, Path', System, parseAbsDir, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Data as Data
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.Process (runNodeCommandAsJob)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase, whenM)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term
import qualified Wasp.Version as WV

data ProjectInfo = ProjectInfo
  { _projectName :: String,
    _appName :: String,
    _template :: Maybe String
  }

createNewProject :: [String] -> Command ()
createNewProject newArgs = do
  projectInfo <- parseProjectInfo newArgs
  createWaspProjectDir projectInfo
  liftIO $ printGettingStartedInstructions $ _projectName projectInfo
  where
    printGettingStartedInstructions :: String -> IO ()
    printGettingStartedInstructions projectName = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectName ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectName)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

parseProjectInfo :: [String] -> Command ProjectInfo
parseProjectInfo newArgs = case newArgs of
  [projectName] -> createProjectInfo projectName Nothing
  [projectName, templateFlag, templateName] | templateFlag `elem` ["--template", "-t"] -> createProjectInfo projectName (Just templateName)
  [_, templateFlag] | templateFlag `elem` ["--template", "-t"] -> throwProjectCreationError "You must provide a template name."
  _anyOtherArgs -> throwProjectCreationError "Invalid arguments for 'wasp new' command."

createProjectInfo :: String -> Maybe String -> Command ProjectInfo
createProjectInfo name template
  | isValidWaspIdentifier appName = return $ ProjectInfo {_projectName = name, _appName = appName, _template = template}
  | otherwise =
      throwProjectCreationError $
        intercalate
          "\n"
          [ "The project's name is not in the valid format!",
            indent 2 "- It can start with a letter or an underscore.",
            indent 2 "- It can contain only letters, numbers, dashes, or underscores.",
            indent 2 "- It can't be a Wasp keyword."
          ]
  where
    appName = kebabToCamelCase name

createWaspProjectDir :: ProjectInfo -> Command ()
createWaspProjectDir projectInfo@ProjectInfo {_template = template} = do
  absWaspProjectDir <- getAbsoluteWaspProjectDir projectInfo
  dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

  when
    dirExists
    $ throwProjectCreationError $
      show absWaspProjectDir ++ " is an existing directory"

  createProjectFromProjectInfo absWaspProjectDir
  where
    createProjectFromProjectInfo absWaspProjectDir = do
      if isJust template
        then createProjectFromTemplate absWaspProjectDir projectInfo
        else liftIO $ do
          initializeProjectFromSkeleton absWaspProjectDir
          writeMainWaspFile absWaspProjectDir projectInfo

getAbsoluteWaspProjectDir :: ProjectInfo -> Command (Path System Abs (Dir WaspProjectDir))
getAbsoluteWaspProjectDir (ProjectInfo projectName _ _) = do
  absCwd <- liftIO getCurrentDirectory
  case parseAbsDir $ absCwd FP.</> projectName of
    Right sp -> return sp
    Left err ->
      throwProjectCreationError $
        "Failed to parse absolute path to wasp project dir: " ++ show err

-- Copies prepared files to the new project directory.
initializeProjectFromSkeleton :: Path' Abs (Dir WaspProjectDir) -> IO ()
initializeProjectFromSkeleton absWaspProjectDir = do
  dataDir <- Data.getAbsDataDirPath
  let absSkeletonDir = dataDir </> [reldir|Cli/templates/new|]
  copyDirRecur (toPathAbsDir absSkeletonDir) (toPathAbsDir absWaspProjectDir)

writeMainWaspFile :: Path System Abs (Dir WaspProjectDir) -> ProjectInfo -> IO ()
writeMainWaspFile waspProjectDir (ProjectInfo projectName appName _) = IOUtil.writeFile absMainWaspFile mainWaspFileContent
  where
    absMainWaspFile = waspProjectDir </> [relfile|main.wasp|]
    mainWaspFileContent =
      unlines
        [ "app %s {" `printf` appName,
          "  wasp: {",
          "    version: \"%s\"" `printf` waspVersionStr,
          "  },",
          "  title: \"%s\"" `printf` projectName,
          "}",
          "",
          "route RootRoute { path: \"/\", to: MainPage }",
          "page MainPage {",
          "  component: import Main from \"@client/MainPage.jsx\"",
          "}"
        ]

createProjectFromTemplate :: Path System Abs (Dir WaspProjectDir) -> ProjectInfo -> Command ()
createProjectFromTemplate absWaspProjectDir ProjectInfo {_appName = appName, _projectName = projectName, _template = maybeTemplateName} = do
  cliSendMessageC $ Msg.Start "Creating project from template..."

  dirWhereProjectIsCreated <- getDirWhereProjectIsCreated
  templatePath <- getPathToRemoteTemplate maybeTemplateName

  let projectDir = projectName

  fetchTemplate templatePath projectDir dirWhereProjectIsCreated
  ensureTemplateWasFetched

  replacePlaceholdersInWaspFile
  where
    getDirWhereProjectIsCreated :: Command (Path System Abs (Dir dir))
    getDirWhereProjectIsCreated = do
      (liftIO getCurrentDirectory <&> parseAbsDir) >>= \case
        Left err -> throwProjectCreationError $ "Failed to parse absolute path to current working directory: " ++ show err
        Right result -> return result

    getPathToRemoteTemplate :: Maybe String -> Command String
    getPathToRemoteTemplate = \case
      Just templateName -> return $ waspTemplatesRepo ++ "/" ++ templateName
      Nothing -> throwProjectCreationError "Template name is not provided."
      where
        -- gh: prefix means Github repo
        waspTemplatesRepo = "gh:wasp-lang/starters"

    fetchTemplate :: String -> String -> Path System Abs (Dir dir) -> Command ()
    fetchTemplate templatePath projectDir dirWhereProjectIsCreated = do
      liftIO executeCmd >>= \case
        ExitSuccess -> return ()
        ExitFailure _ -> throwProjectCreationError "Failed to create project from template."
      where
        executeCmd = newChan >>= runNodeCommandAsJob dirWhereProjectIsCreated "npx" args J.Cli
        args = ["giget@latest", templatePath, projectDir]

    -- gitget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
    ensureTemplateWasFetched :: Command ()
    ensureTemplateWasFetched = do
      whenM
        (liftIO $ IOUtil.isDirectoryEmpty absWaspProjectDir)
        $ throwProjectCreationError "Are you sure that the template exists? 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

    replacePlaceholdersInWaspFile :: Command ()
    replacePlaceholdersInWaspFile = liftIO $ do
      mainWaspFileContent <- IOUtil.readFileStrict absMainWaspFile

      let replacedContent =
            foldl
              (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc)
              mainWaspFileContent
              placeholders

      IOUtil.writeFileFromText absMainWaspFile replacedContent
      where
        absMainWaspFile = absWaspProjectDir </> [relfile|main.wasp|]
        placeholders =
          [ ("__waspAppName__", appName),
            ("__waspProjectName__", projectName),
            ("__waspVersion__", waspVersionStr)
          ]

waspVersionStr :: String
waspVersionStr = "^%s" `printf` show WV.waspVersion

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"
