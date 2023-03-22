module Wasp.Generator.FileDraft.CopyAndModifyTextFileDraft
  ( CopyAndModifyTextFileDraft (..),
  )
where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, File', Path', Rel, (</>))
import qualified StrongPath as SP
import System.IO.Error (doesNotExistErrorType, mkIOError)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad
import Wasp.Util (checksumFromText)

-- | File draft based on existing text file + some modifications.
data CopyAndModifyTextFileDraft = CopyAndModifyTextFileDraft
  { -- | Path where the file will be copied to.
    _dstPath :: !(Path' (Rel ProjectRootDir) File'),
    -- | Absolute path of source file to copy.
    _srcPath :: !(Path' Abs File'),
    _failIfSrcDoesNotExist :: !Bool,
    _modify :: !(T.Text -> T.Text)
  }

instance Writeable CopyAndModifyTextFileDraft where
  write absDstDirPath draft = do
    srcFileExists <- doesFileExist (SP.fromAbsFile $ _srcPath draft)
    if srcFileExists
      then do
        createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent absDraftDstPath)
        readFileAsText (SP.fromAbsFile $ _srcPath draft)
          >>= writeFileFromText (SP.fromAbsFile absDraftDstPath) . _modify draft
      else
        when
          (_failIfSrcDoesNotExist draft)
          ( throwIO $
              mkIOError
                doesNotExistErrorType
                "Source file of CopyTextFileDraft does not exist."
                Nothing
                (Just $ SP.fromAbsFile $ _srcPath draft)
          )
    where
      absDraftDstPath = absDstDirPath </> _dstPath draft

  -- NOTE: We are reading file here, but then also again later in the "write", so
  --   we are reading file at least twice! Any way to avoid it?
  --   Idea: We could cache it in an mvar in the CopyTextFileDraft.
  getChecksum draft =
    checksumFromText . _modify draft <$> T.IO.readFile (SP.fromAbsFile $ _srcPath draft)

  getDstPath draft = Left $ _dstPath draft
