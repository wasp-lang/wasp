module Wasp.Util.StrongPath
  ( replaceExtension,
    stripProperPrefix,
  )
where

import Control.Monad.Catch (MonadThrow)
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP

stripProperPrefix :: SP.Path' SP.Abs (SP.Dir a) -> SP.Path' SP.Abs (SP.File b) -> Maybe (SP.Path' (SP.Rel a) (SP.File b))
stripProperPrefix base file =
  SP.fromPathRelFile
    <$> P.stripProperPrefix (SP.toPathAbsDir base) (SP.toPathAbsFile file)

replaceExtension :: MonadThrow m => SP.Path' SP.Abs (SP.File a) -> String -> m (SP.Path' SP.Abs (SP.File a))
replaceExtension path ext =
  SP.fromPathAbsFile <$> P.replaceExtension ext (SP.toPathAbsFile path)
