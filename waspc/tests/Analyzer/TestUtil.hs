module Analyzer.TestUtil where

import Wasp.Analyzer.Ctx (Ctx, WithCtx (..), ctxFromRgn)
import qualified Wasp.Analyzer.Ctx as Ctx
import Wasp.Analyzer.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.SourceRegion (SourceRegion (..))
import qualified Wasp.Analyzer.TypeChecker as T

pos :: Int -> Int -> SourcePosition
pos = SourcePosition

rgn :: (Int, Int) -> (Int, Int) -> SourceRegion
rgn (sl, sc) (el, ec) = SourceRegion (pos sl sc) (pos el ec)

ctx :: (Int, Int) -> (Int, Int) -> Ctx
ctx (a, b) (c, d) = ctxFromRgn (pos a b) (pos c d)

wctx :: (Int, Int) -> (Int, Int) -> a -> WithCtx a
wctx start end = WithCtx (ctx start end)

fromWithCtx :: WithCtx T.TypedExpr -> T.TypedExpr
fromWithCtx = Ctx.fromWithCtx
