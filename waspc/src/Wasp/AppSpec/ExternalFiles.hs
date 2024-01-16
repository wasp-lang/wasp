{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.ExternalFiles
  ( -- | Wasp project consists of Wasp code (.wasp files) and external files.
    -- External files can be either:
    --  - External code files (e.g., JS, TS) used/referenced by Wasp code. These
    --    files reside in the \/src directory which we call the "source external code dir".
    --  - External (static) public files served unaltered by Wasp. These
    --    files reside in the \/public directory which we call the "source external public dir".
    -- We call these directories "source" directories because they're found in
    -- the project's source dir, not in the generated project's source dir.
    --
    -- Therefore, the whole specification of the web app is not just Wasp code, but a combination of
    -- Wasp code and external files.
    CodeFile (..),
    PublicFile (..),
    filePathInExtCodeDir,
    fileAbsPath,
    fileText,
    SourceExternalCodeDir,
    SourceExternalPublicDir,
  )
where

import Data.Data (Data)
import Data.Text (Text)
import qualified Data.Text.Lazy as TextL
import StrongPath (Abs, Dir, File', Path', Rel, (</>))

-- | Directory in the Wasp project that contains external code.
--   External code files are obtained from it.
data SourceExternalCodeDir deriving (Data)

-- | Directory in Wasp project that contains external public static files.
--   Public files are obtained from it.
data SourceExternalPublicDir deriving (Data)

data CodeFile = CodeFile
  { _pathInExtCodeDir :: !(Path' (Rel SourceExternalCodeDir) File'),
    _extCodeDirPath :: !(Path' Abs (Dir SourceExternalCodeDir)),
    -- | File content. Since it is lazy, it might throw error when evaluated,
    -- since reading will happen only then. E.g. it will throw error if file is not a textual file.
    _text :: TextL.Text
  }

data PublicFile = PublicFile
  { _pathInPublicDir :: !(Path' (Rel SourceExternalPublicDir) File'),
    _publicDirPath :: !(Path' Abs (Dir SourceExternalPublicDir))
  }

instance Show CodeFile where
  show = show . _pathInExtCodeDir

instance Eq CodeFile where
  f1 == f2 = _pathInExtCodeDir f1 == _pathInExtCodeDir f2

-- | Returns path relative to the external code directory.
filePathInExtCodeDir :: CodeFile -> Path' (Rel SourceExternalCodeDir) File'
filePathInExtCodeDir = _pathInExtCodeDir

-- | Unsafe method: throws error if text could not be read (if file is not a textual file)!
fileText :: CodeFile -> Text
fileText = TextL.toStrict . _text

-- | Returns absolute path of the external code file.
fileAbsPath :: CodeFile -> Path' Abs File'
fileAbsPath file = _extCodeDirPath file </> _pathInExtCodeDir file
