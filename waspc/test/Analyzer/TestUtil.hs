module Analyzer.TestUtil where

import qualified Wasp.Analyzer.Parser as P
import qualified Wasp.Analyzer.TypeChecker as T

pos :: Int -> Int -> P.SourcePosition
pos line column = P.SourcePosition line column

ctx :: Int -> Int -> P.Ctx
ctx line column = P.ctxFromPos $ P.SourcePosition line column

wctx :: Int -> Int -> a -> P.WithCtx a
wctx line column = P.WithCtx (ctx line column)

fromWithCtx :: P.WithCtx T.TypedExpr -> T.TypedExpr
fromWithCtx = P.fromWithCtx
