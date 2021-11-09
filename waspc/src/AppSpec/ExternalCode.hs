module AppSpec.ExternalCode
  ( File (..),
    filePathInExtCodeDir,
    fileAbsPath,
    fileText,
    SourceExternalCodeDir,
  )
where

import Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import StrongPath (Abs, Dir, File', Path', Rel, (</>))

-- | Directory in Wasp source that contains external code.
--   External code files are obtained from it.
data SourceExternalCodeDir

data File = File
  { _pathInExtCodeDir :: !(Path' (Rel SourceExternalCodeDir) File'),
    _extCodeDirPath :: !(Path' Abs (Dir SourceExternalCodeDir)),
    -- | File content. It will throw error when evaluated if file is not textual file.
    _text :: TextL.Text
  }

instance Show File where
  show = show . _pathInExtCodeDir

instance Eq File where
  f1 == f2 = _pathInExtCodeDir f1 == _pathInExtCodeDir f2

-- | Returns path relative to the external code directory.
filePathInExtCodeDir :: File -> Path' (Rel SourceExternalCodeDir) File'
filePathInExtCodeDir = _pathInExtCodeDir

-- | Unsafe method: throws error if text could not be read (if file is not a textual file)!
fileText :: File -> Text
fileText = TextL.toStrict . _text

-- | Returns absolute path of the external code file.
fileAbsPath :: File -> Path' Abs File'
fileAbsPath file = _extCodeDirPath file </> _pathInExtCodeDir file
