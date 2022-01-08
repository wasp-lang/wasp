{-# LANGUAGE DeriveFunctor #-}

module Wasp.Analyzer.Parser.Ctx
  ( WithCtx (..),
    withCtx,
    Ctx (..),
    ctxFromPos,
    ctxFromRgn,
    getCtxRgn,
    fromWithCtx,
  )
where

import Wasp.Analyzer.Parser.SourcePosition (SourcePosition)
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))

data WithCtx a = WithCtx Ctx a
  deriving (Eq, Show, Functor)

withCtx :: (Ctx -> a -> b) -> WithCtx a -> b
withCtx f (WithCtx ctx x) = f ctx x

-- | Gives parsing context to AST nodes -> e.g. source region from which they originated.
data Ctx = Ctx
  { ctxSourceRegion :: SourceRegion
  }
  deriving (Show, Eq)

ctxFromPos :: SourcePosition -> Ctx
ctxFromPos pos = Ctx {ctxSourceRegion = SourceRegion pos pos}

ctxFromRgn :: SourcePosition -> SourcePosition -> Ctx
ctxFromRgn posStart posEnd = Ctx {ctxSourceRegion = SourceRegion posStart posEnd}

getCtxRgn :: Ctx -> SourceRegion
getCtxRgn = ctxSourceRegion

fromWithCtx :: WithCtx a -> a
fromWithCtx (WithCtx _ a) = a
