{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.AttachedComment
  ( AttachedComment (..),
    AttachedCommentLocation (..),
    CanAttachComments (..),
  )
where

import Data.Data (Data)

class CanAttachComments a where
  attachComment :: a -> AttachedComment -> a

data AttachedComment = AttachedComment
  { _location :: AttachedCommentLocation,
    _text :: String
  }
  deriving (Show, Eq, Data)

data AttachedCommentLocation = LineBefore | EndOfLine
  deriving (Show, Eq, Data)
