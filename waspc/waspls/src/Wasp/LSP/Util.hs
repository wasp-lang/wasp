module Wasp.LSP.Util
  ( allP,
    anyP,
    hoistMaybe,
    waspSourceRegionToLspRange,
    waspPositionToLspPosition,
    getPathRelativeToProjectDir,
    lspUriToPath,
    getWaspDirFromWaspFileUri,
  )
where

import Control.Lens ((+~))
import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Function ((&))
import Data.List (stripPrefix)
import qualified Data.Text as T
import qualified Language.LSP.Types as LSP hiding (line)
import qualified Language.LSP.Types.Lens as LSP
import qualified StrongPath as SP
import qualified Wasp.Analyzer.Parser as W
import qualified Wasp.Analyzer.Parser.SourceRegion as W
import Wasp.LSP.ServerMonads.HasProjectRootDir (HasProjectRootDir (getProjectRootDir))
import Wasp.Project (WaspProjectDir)
import Wasp.Util.StrongPath (stripProperPrefix)

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

-- | @getPathRelativeToProjectDir file@ finds the path to @file@ if it is inside the
-- project root directory.
getPathRelativeToProjectDir :: HasProjectRootDir m => SP.Path' SP.Abs (SP.File a) -> m (Maybe (SP.Path' (SP.Rel WaspProjectDir) (SP.File a)))
getPathRelativeToProjectDir file = do
  maybeProjectRootDir <- getProjectRootDir
  case maybeProjectRootDir of
    Nothing -> pure Nothing
    Just projectRootDir -> pure $ stripProperPrefix projectRootDir file

getWaspDirFromWaspFileUri :: LSP.Uri -> Maybe (SP.Path' SP.Abs (SP.Dir WaspProjectDir))
getWaspDirFromWaspFileUri uri = lspUriToPath uri >>= Just . SP.parent

lspUriToPath :: LSP.Uri -> Maybe (SP.Path' SP.Abs (SP.File ()))
lspUriToPath = SP.parseAbsFile <=< stripPrefix "file://" . T.unpack . LSP.getUri
