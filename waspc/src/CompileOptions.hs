module CompileOptions
    ( CompileOptions(..)
    ) where

import StrongPath (Path, Abs, Dir, File)
import ExternalCode(SourceExternalCodeDir)


-- TODO(martin): Should these be merged with Wasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
    { externalCodeDirPath :: !(Path Abs (Dir SourceExternalCodeDir))
    , waspignoreFilePath :: !(Path Abs File)
    , isBuild :: !Bool
    }
