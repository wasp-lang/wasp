module Analyzer.TestUtil where

import qualified Wasp.Analyzer.Parser as P
import qualified Wasp.Analyzer.TypeChecker as T

pos :: Int -> Int -> P.SourcePosition
pos line column = P.SourcePosition line column

ctx :: (Int, Int) -> (Int, Int) -> P.Ctx
ctx (a, b) (c, d) = P.ctxFromRgn (pos a b) (pos c d)

wctx :: (Int, Int) -> (Int, Int) -> a -> P.WithCtx a
wctx start end = P.WithCtx (ctx start end)

fromWithCtx :: P.WithCtx T.TypedExpr -> T.TypedExpr
fromWithCtx = P.fromWithCtx
