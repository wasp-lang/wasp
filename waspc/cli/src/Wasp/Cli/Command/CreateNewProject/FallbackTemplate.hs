module Wasp.Cli.Command.CreateNewProject.FallbackTemplate where

import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Path', reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import Text.Printf (printf)
import Wasp.Cli.Command.CreateNewProject.Common (waspVersionBounds)
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

createProjectFromFallbackTemplate :: Path' Abs (Dir WaspProjectDir) -> String -> String -> IO ()
createProjectFromFallbackTemplate absWaspProjectDir projectName appName = do
  copyFallbackTemplateToNewProjectDir
  writeMainWaspFile
  where
    copyFallbackTemplateToNewProjectDir :: IO ()
    copyFallbackTemplateToNewProjectDir = do
      dataDir <- Data.getAbsDataDirPath
      let absFallbackTemplateDir = dataDir </> [reldir|Cli/templates/new|]
      copyDirRecur (toPathAbsDir absFallbackTemplateDir) (toPathAbsDir absWaspProjectDir)

    writeMainWaspFile :: IO ()
    writeMainWaspFile = IOUtil.writeFile absMainWaspFile mainWaspFileContent
      where
        absMainWaspFile = absWaspProjectDir </> [relfile|main.wasp|]
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
