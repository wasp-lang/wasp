module Analyzer.TestUtil where

import Wasp.Analyzer.Ctx (Ctx, WithCtx (..), ctxFromRgn)
import qualified Wasp.Analyzer.Ctx as Ctx
import Wasp.Analyzer.SourcePosition (SourcePosition (..))
import qualified Wasp.Analyzer.TypeChecker as T

pos :: Int -> Int -> SourcePosition
pos = SourcePosition

ctx :: (Int, Int) -> (Int, Int) -> Ctx
ctx (a, b) (c, d) = ctxFromRgn (pos a b) (pos c d)

fromWithCtx :: WithCtx T.TypedExpr -> T.TypedExpr
fromWithCtx = Ctx.fromWithCtx
