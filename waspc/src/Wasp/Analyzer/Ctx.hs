{-# LANGUAGE DeriveFunctor #-}

module Wasp.Analyzer.Ctx
  ( WithCtx (..),
    withCtx,
    Ctx (..),
    ctxFromRgn,
    fromWithCtx,
  )
where

import Wasp.Analyzer.SourcePosition (SourcePosition)
import Wasp.Analyzer.SourceRegion (SourceRegion (..))

data WithCtx a = WithCtx Ctx a
  deriving (Eq, Show, Functor)

withCtx :: (Ctx -> a -> b) -> WithCtx a -> b
withCtx f (WithCtx ctx x) = f ctx x

-- | Gives parsing context to AST nodes -> e.g. source region from which they originated.
data Ctx = Ctx
  { ctxSourceRegion :: SourceRegion
  }
  deriving (Show, Eq)

ctxFromRgn :: SourcePosition -> SourcePosition -> Ctx
ctxFromRgn posStart posEnd = Ctx {ctxSourceRegion = SourceRegion posStart posEnd}

fromWithCtx :: WithCtx a -> a
fromWithCtx (WithCtx _ a) = a
