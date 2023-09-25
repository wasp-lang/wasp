module Wasp.Cli.Command.UseRecipe.Common where

import Control.Monad (unless)
import StrongPath (Abs, Dir, File, Path', Rel, reldir, toFilePath, (</>))
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import qualified Wasp.Data

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

  -- _ <- error $ "Copying " <> pathInRecipesDirStr <> " to " <> pathInProjectDirStr

  isExistingFile <- doesFileExist pathInProjectDirStr
  unless isExistingFile $ do
    createDirectoryIfMissing True $ takeDirectory pathInProjectDirStr
    copyFile pathInRecipesDirStr pathInProjectDirStr
