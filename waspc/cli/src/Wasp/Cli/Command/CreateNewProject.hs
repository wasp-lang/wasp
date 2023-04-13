{-# LANGUAGE TypeOperators #-}

module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Path.IO (copyDirRecur, doesDirExist)
import StrongPath (Abs, Dir, Path, Path', System, parseAbsDir, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Console.Wizard (Line, Wizard, defaultTo, inRange, line, nonEmpty, parseRead, retry, run, (:<:))
import System.Console.Wizard.BasicIO (basicIO)
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import System.Process (callCommand)
import Text.Printf (printf)
import UnliftIO.Exception (SomeException, try)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (Arguments, ProjectName)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Data as Data
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (indent, kebabToCamelCase, whenM)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term
import qualified Wasp.Version as WV

data ProjectInfo = ProjectInfo
  { _projectName :: String,
    _appName :: String,
    _templateName :: Maybe String
  }

createNewProject :: ProjectName -> Arguments -> Command ()
createNewProject projectName newArgs = do
  liftIO $ putStrLn "Welcome to Wasp!"
  student <- liftIO runTestWixards
  liftIO $ print student
  projectInfo <- parseProjectInfo projectName newArgs
  -- createWaspProjectDir projectInfo
  liftIO $ printGettingStartedInstructions $ _projectName projectInfo
  where
    printGettingStartedInstructions :: ProjectName -> IO ()
    printGettingStartedInstructions projectFolder = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectFolder ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectFolder)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

parseProjectInfo :: ProjectName -> Arguments -> Command ProjectInfo
parseProjectInfo projectName newArgs = case newArgs of
  [] -> createProjectInfo projectName Nothing
  [templateFlag, templateName] | templateFlag `elem` ["--template", "-t"] -> createProjectInfo projectName (Just templateName)
  [templateFlag] | templateFlag `elem` ["--template", "-t"] -> throwProjectCreationError "You must provide a template name."
  _anyOtherArgs -> throwProjectCreationError "Invalid arguments for 'wasp new' command."

createProjectInfo :: ProjectName -> Maybe String -> Command ProjectInfo
createProjectInfo name templateName
  | isValidWaspIdentifier appName = return $ ProjectInfo {_projectName = name, _appName = appName, _templateName = templateName}
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
createWaspProjectDir projectInfo@ProjectInfo {_templateName = template} = do
  absWaspProjectDir <- getAbsoluteWaspProjectDir projectInfo
  dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir

  when dirExists $
    throwProjectCreationError $
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
          "    version: \"%s\"" `printf` waspVersionBounds,
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
createProjectFromTemplate absWaspProjectDir ProjectInfo {_appName = appName, _projectName = projectName, _templateName = maybeTemplateName} = do
  cliSendMessageC $ Msg.Start "Creating project from template..."

  templatePath <- getPathToRemoteTemplate maybeTemplateName

  let projectDir = projectName

  fetchTemplate templatePath projectDir
  ensureTemplateWasFetched

  replacePlaceholdersInWaspFile
  where
    getPathToRemoteTemplate :: Maybe String -> Command String
    getPathToRemoteTemplate = \case
      Just templateName -> return $ waspTemplatesRepo ++ "/" ++ templateName
      Nothing -> throwProjectCreationError "Template name is not provided."
      where
        -- gh: prefix means Github repo
        waspTemplatesRepo = "gh:wasp-lang/starters"

    fetchTemplate :: String -> String -> Command ()
    fetchTemplate templatePath projectDir = do
      liftIO (try executeCmd) >>= \case
        Left (e :: SomeException) -> throwProjectCreationError $ "Failed to create project from template: " ++ show e
        Right _ -> return ()
      where
        executeCmd = callCommand $ unwords command
        command = ["npx", "giget@latest", templatePath, projectDir]

    -- gitget doesn't fail if the template dir doesn't exist in the repo, so we need to check if the directory exists.
    ensureTemplateWasFetched :: Command ()
    ensureTemplateWasFetched =
      whenM (liftIO $ IOUtil.isDirectoryEmpty absWaspProjectDir) $
        throwProjectCreationError "Are you sure that the template exists? 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

    replacePlaceholdersInWaspFile :: Command ()
    replacePlaceholdersInWaspFile = liftIO $ do
      mainWaspFileContent <- IOUtil.readFileStrict absMainWaspFile

      let replacedContent =
            foldl
              (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc)
              mainWaspFileContent
              replacements

      IOUtil.writeFileFromText absMainWaspFile replacedContent
      where
        absMainWaspFile = absWaspProjectDir </> [relfile|main.wasp|]
        replacements =
          [ ("__waspAppName__", appName),
            ("__waspProjectName__", projectName),
            ("__waspVersion__", waspVersionBounds)
          ]

waspVersionBounds :: String
waspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

type Name = String

type Class = Int

data Student = Student Name Class deriving (Show)

nameWizard :: (Line :<: b) => Wizard b Name
nameWizard = retry $ nonEmpty $ line "Name: "

classWizard :: (Line :<: b) => Wizard b Class
classWizard =
  retry $
    inRange (1, 5) $
      parseRead $
        nonEmpty (line "Class[1]: ") `defaultTo` "1"

studentWizard :: (Line :<: b) => Wizard b Student
studentWizard = Student <$> nameWizard <*> classWizard

runTestWixards :: IO (Maybe Student)
runTestWixards = run (basicIO studentWizard)
