{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.ExternalCode
  ( -- | Wasp project consists of Wasp code (.wasp files) and external code (e.g. .js files) that is
    -- used/referenced by the Wasp code.
    -- Therefore, the whole specification of the web app is not just Wasp code, but a combination of
    -- Wasp code and external code.
    -- Main unit of external code is File, and external code is currently all organized in a single
    -- directory in Wasp project which we call source external code dir (source because it is in the
    -- Wasp project \/ source dir, and not in the generated project \/ source).
    File (..),
    filePathInExtCodeDir,
    fileAbsPath,
    fileText,
    SourceExternalCodeDir,
  )
where

import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import StrongPath (Abs, Dir, File', Path', Rel, (</>))

-- | Directory in Wasp source that contains external code.
--   External code files are obtained from it.
data SourceExternalCodeDir deriving (Data)

data File = File
  { _pathInExtCodeDir :: !(Path' (Rel SourceExternalCodeDir) File'),
    _extCodeDirPath :: !(Path' Abs (Dir SourceExternalCodeDir)),
    -- | File content. Since it is lazy, it might throw error when evaluated,
    -- since reading will happen only then. E.g. it will throw error if file is not a textual file.
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
