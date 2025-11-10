module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local
  ( createProjectOnDiskFromLocalTemplate,
  )
where

import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Dir', Path')
import StrongPath.Path (toPathAbsDir)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromLocalTemplate ::
  Path' Abs (Dir WaspProjectDir) -> NewProjectName -> NewProjectAppName -> Path' Abs Dir' -> IO ()
createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName templatePath = do
  copyDirRecur (toPathAbsDir templatePath) (toPathAbsDir absWaspProjectDir)
  replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir
