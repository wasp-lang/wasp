module Wasp.LSP.Util
  ( allP,
    anyP,
    hoistMaybe,
    waspSourceRegionToLspRange,
    waspPositionToLspPosition,
    stripProperPrefix,
    absFileInProjectRootDir,
  )
where

import Control.Lens ((+~))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Function ((&))
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP hiding (line)
import qualified Language.LSP.Types.Lens as LSP
import qualified Path as P
import qualified StrongPath as SP
import qualified StrongPath.Path as SP
import qualified Wasp.Analyzer.Parser as W
import qualified Wasp.Analyzer.Parser.SourceRegion as W
import Wasp.Project (WaspProjectDir)

waspSourceRegionToLspRange :: W.SourceRegion -> LSP.Range
waspSourceRegionToLspRange rgn =
  LSP.Range
    { _start = waspPositionToLspPosition (W.getRgnStart rgn),
      _end = waspPositionToLspPosition (W.getRgnEnd rgn) & LSP.character +~ 1
    }

waspPositionToLspPosition :: W.SourcePosition -> LSP.Position
waspPositionToLspPosition (W.SourcePosition ln col) =
  LSP.Position
    { _line = fromIntegral ln - 1,
      _character = fromIntegral col - 1
    }

-- | Check if all the supplied predicates are true.
allP :: Foldable f => f (a -> Bool) -> a -> Bool
allP preds x = all ($ x) preds

-- | Check if any of the supplied predicates are true.
anyP :: Foldable f => f (a -> Bool) -> a -> Bool
anyP preds x = any ($ x) preds

-- | Lift a 'Maybe' into a 'MaybeT' monad transformer.
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

stripProperPrefix :: SP.Path' SP.Abs (SP.Dir a) -> SP.Path' SP.Abs (SP.File b) -> Maybe (SP.Path' (SP.Rel a) (SP.File b))
stripProperPrefix base file =
  SP.fromPathRelFile
    <$> P.stripProperPrefix (SP.toPathAbsDir base) (SP.toPathAbsFile file)

-- | @absFileInProjectRootDir file@ finds the path to @file@ if it is inside the
-- project root directory.
absFileInProjectRootDir :: LSP.MonadLsp c m => SP.Path' SP.Abs (SP.File a) -> m (Maybe (SP.Path' (SP.Rel WaspProjectDir) (SP.File a)))
absFileInProjectRootDir file = do
  maybeProjectRootDir <- (>>= SP.parseAbsDir) <$> LSP.getRootPath
  case maybeProjectRootDir of
    Nothing -> pure Nothing
    Just projectRootDir -> pure $ stripProperPrefix projectRootDir file
