module CompileOptions
    ( CompileOptions(..)
    ) where

import qualified Path.Aliases as Path

-- TODO(martin): Should these be merged with Wasp data? Is it really a separate thing or not?
--   It would be easier to pass around if it is part of Wasp data. But is it semantically correct?
--   Maybe it is, even more than this!
data CompileOptions = CompileOptions
    { externalCodeDirPath :: !Path.AbsDir
    }
