module Generator.ExternalCodeGenerator.Common
    ( ExternalCodeGeneratorStrategy(..)
    ) where

import Data.Text (Text)
import qualified Path.Aliases as Path


data ExternalCodeGeneratorStrategy = ExternalCodeGeneratorStrategy
    { -- | Takes a file path where the external code js file will be generated (relative to the generated external code dir).
      -- Also takes text of the file. Returns text where special @wasp imports have been replaced with
      -- imports that will work.
      _resolveJsFileWaspImports :: Path.RelFile -> Text -> Text
      -- | Path to directory where ext code will be generated. Relative to the generated project root.
    , _extCodeDirInProjectRootDir :: Path.RelDir
    }



