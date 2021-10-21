module AST
  ( App (..),
    AuthMethod (..),
    Page (..),
  )
where

import AST.Core.Ref (Ref)

data App = App
  { title :: String,
    authMethod :: AuthMethod,
    defaultPage :: Ref Page
  }
  deriving (Show, Eq)

data AuthMethod = EmailAndPassword deriving (Show, Eq)

data Page = Page
  { content :: String
  }
  deriving (Show, Eq)
