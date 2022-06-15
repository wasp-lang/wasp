module Wasp.LSP.Util (waspSourceRegionToRange) where

import Control.Lens ((+~))
import Data.Function ((&))
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Wasp.Analyzer.Parser as W
import qualified Wasp.Analyzer.Parser.SourceRegion as W

waspSourceRegionToRange :: W.SourceRegion -> LSP.Range
waspSourceRegionToRange rgn =
  LSP.Range
    { _start = waspPosToPos (W.getRgnStart rgn),
      _end = waspPosToPos (W.getRgnEnd rgn) & LSP.character +~ 1
    }

waspPosToPos :: W.SourcePosition -> LSP.Position
waspPosToPos (W.SourcePosition ln col) =
  LSP.Position
    { _line = fromIntegral ln - 1,
      _character = fromIntegral col - 1
    }
