module Wasp.LSP.Util
  ( waspSourceRegionToLspRange,
    waspPositionToLspPosition,
    hoistMaybe,
  )
where

import Control.Lens ((+~))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Function ((&))
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Wasp.Analyzer.Parser as W
import qualified Wasp.Analyzer.Parser.SourceRegion as W

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

-- | Lift a 'Maybe' into a 'MaybeT' monad transformer.
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
