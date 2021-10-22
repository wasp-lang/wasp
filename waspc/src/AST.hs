module AST
  ( App (..),
    AuthMethod (..),
    Page (..),
  )
where

import AST.Core.Ref (Ref)

-- TODO: Make AST full-featured, so it supports latest version of waps-lang.

-- TODO: Split into multiple files (one for App, one for Page, ...).

-- TODO: I should probably move Decl out of Analyzer and make it part of the AST here,
--   since currently AST has no "top structure", right now it is just a bunch of nodes,
--   and Decl is what wraps it all together, which really makes it a part of AST, not Analyzer.
--   Also, in order for Ref to make sense, we need Decl since it contains names, so that
--   again suggests that we need to make Decl part of AST.

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
