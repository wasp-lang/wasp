module Wasp.Project.ExternalFiles
  ( readPublicFiles,
    readCodeFiles,
  )
where

import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.IO as TextL.IO
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)
import Wasp.AppSpec.ExternalFiles (CodeFile (CodeFile), PublicFile (PublicFile))
import Wasp.Project.Common (WaspProjectDir, extPublicDirInWaspProjectDir, srcDirInWaspProjectDir)
import Wasp.Project.Waspignore (getNotIgnoredRelFilePaths, waspIgnorePathInWaspProjectDir)

-- | Returns all files contained in the specified ext public dir
--   except files ignores by the specified waspignore file.
readPublicFiles :: Path' Abs (Dir WaspProjectDir) -> IO [PublicFile]
readPublicFiles waspProjectDir = do
  let externalPublicDirPath = waspProjectDir </> extPublicDirInWaspProjectDir
  let waspignoreFilePath = waspProjectDir </> waspIgnorePathInWaspProjectDir
  relFilePaths <- getNotIgnoredRelFilePaths waspignoreFilePath externalPublicDirPath
  return $ map (`PublicFile` externalPublicDirPath) relFilePaths

-- | Returns all files contained in the specified ext code dir
--   except files ignores by the specified waspignore file.
readCodeFiles :: Path' Abs (Dir WaspProjectDir) -> IO [CodeFile]
readCodeFiles waspProjectDir = do
  let externalCodeDirPath = waspProjectDir </> srcDirInWaspProjectDir
  let waspignoreFilePath = waspProjectDir </> waspIgnorePathInWaspProjectDir
  relFilePaths <- getNotIgnoredRelFilePaths waspignoreFilePath externalCodeDirPath
  let absFiles = map (externalCodeDirPath </>) relFilePaths
  -- NOTE: We read text from all the files, regardless if they are text files or not, because
  --   we don't know if they are a text file or not.
  --   Since we do lazy reading (Text.Lazy), this is not a problem as long as we don't try to use
  --   text of a file that is actually not a text file -> then we will get an error when Haskell
  --   actually tries to read that file.
  -- TODO: We are doing lazy IO here, and there is an idea of it being a thing to avoid, due to no
  --   control over when resources are released and similar.
  --   If we do figure out that this is causing us problems, we could do the following refactoring:
  --     Don't read files at this point, just list them, and Wasp will contain just list of filepaths.
  --     Modify TextFileDraft so that it also takes text transformation function (Text -> Text),
  --     or create new file draft that will support that.
  --     In generator, when creating TextFileDraft, give it function/logic for text transformation,
  --     and it will be taken care of when draft will be written to the disk.
  fileTexts <- catMaybes <$> mapM (tryReadFile . SP.toFilePath) absFiles
  let files = zipWith (`CodeFile` externalCodeDirPath) relFilePaths fileTexts
  return files
  where
    -- NOTE(matija): we had cases (e.g. tmp Vim files) where a file initially existed
    -- but then got deleted before actual reading was invoked.
    -- That would make this function crash, so we just ignore those errors.
    tryReadFile :: FilePath -> IO (Maybe TextL.Text)
    tryReadFile fp =
      (Just <$> TextL.IO.readFile fp)
        `catch` ( \e ->
                    if isDoesNotExistError e
                      then return Nothing
                      else throwIO e
                )
