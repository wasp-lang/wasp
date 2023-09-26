module Wasp.Cli.Command.UseRecipe.Common where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, File, Path', Rel, reldir, relfile, toFilePath, (</>))
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import qualified Wasp.Data
import Wasp.Util.IO (appendToFile)

data RecipesDir

-- | Path where the recipes are stored in the data dir.
recipesDirPathInDataDir :: Path' (Rel Wasp.Data.DataDir) (Dir RecipesDir)
recipesDirPathInDataDir = [reldir|Cli/recipes|]

getRecipesDir :: IO (Path' Abs (Dir RecipesDir))
getRecipesDir = (</> recipesDirPathInDataDir) <$> Wasp.Data.getAbsDataDirPath

copyFileIfDoesNotExist ::
  Path' (Rel RecipesDir) (File f) ->
  Path' Abs (File f) ->
  IO ()
copyFileIfDoesNotExist pathInRecipesDir pathInProjectDir = do
  recipesDir <- getRecipesDir
  let pathInProjectDirStr = toFilePath pathInProjectDir
  let pathInRecipesDirStr = toFilePath $ recipesDir </> pathInRecipesDir

  isExistingFile <- doesFileExist pathInProjectDirStr
  unless isExistingFile $ do
    createDirectoryIfMissing True $ takeDirectory pathInProjectDirStr
    copyFile pathInRecipesDirStr pathInProjectDirStr

appendToServerEnv :: String -> Command ()
appendToServerEnv content = do
  InWaspProject waspProjectDir <- require

  let serverEnvPath = waspProjectDir </> [relfile|.env.server|]
  liftIO $ appendToFile serverEnvPath content
