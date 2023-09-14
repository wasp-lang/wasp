module Wasp.Util.StrongPath
  ( replaceExtension,
    replaceRelExtension,
    stripProperPrefix,
    splitAbsExtension,
    splitRelExtension,
  )
where

import Control.Arrow (first)
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

replaceRelExtension :: MonadThrow m => SP.Path' (SP.Rel b) (SP.File a) -> String -> m (SP.Path' (SP.Rel b) (SP.File a))
replaceRelExtension path ext =
  SP.fromPathRelFile <$> P.replaceExtension ext (SP.toPathRelFile path)

splitAbsExtension :: MonadThrow m => SP.Path' SP.Abs (SP.File a) -> m (SP.Path' SP.Abs (SP.File c), String)
splitAbsExtension path =
  first SP.fromPathAbsFile <$> P.splitExtension (SP.toPathAbsFile path)

splitRelExtension :: MonadThrow m => SP.Path' (SP.Rel b) (SP.File a) -> m (SP.Path' (SP.Rel b) (SP.File c), String)
splitRelExtension path =
  first SP.fromPathRelFile <$> P.splitExtension (SP.toPathRelFile path)
