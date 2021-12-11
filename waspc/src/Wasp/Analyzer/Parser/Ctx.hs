{-# LANGUAGE DeriveFunctor #-}

module Wasp.Analyzer.Parser.Ctx
  ( WithCtx (..),
    withCtx,
    Ctx (..),
    ctxFromPos,
    getCtxPos,
    fromWithCtx,
  )
where

import Wasp.Analyzer.Parser.SourcePosition (SourcePosition)

data WithCtx a = WithCtx Ctx a
  deriving (Eq, Show, Functor)

withCtx :: (Ctx -> a -> b) -> WithCtx a -> b
withCtx f (WithCtx ctx x) = f ctx x

-- | Gives parsing context to AST nodes -> e.g. source position from which they originated.
-- TODO: Instead of having just SourcePosition, it would be better to have SourceRegion, since errors
-- usually refer to a region and not just one position/char in the code.
-- This is captured in an issue https://github.com/wasp-lang/wasp/issues/404 .
data Ctx = Ctx
  { ctxSourcePosition :: SourcePosition
  }
  deriving (Show, Eq)

ctxFromPos :: SourcePosition -> Ctx
ctxFromPos pos = Ctx {ctxSourcePosition = pos}

getCtxPos :: Ctx -> SourcePosition
getCtxPos = ctxSourcePosition

fromWithCtx :: WithCtx a -> a
fromWithCtx (WithCtx _ a) = a
