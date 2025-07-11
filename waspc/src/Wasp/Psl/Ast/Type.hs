module Wasp.Psl.Ast.Type
  ( Type (..),
  )
where

import Wasp.Psl.Ast.AttachedComment (AttachedComment)
import Wasp.Psl.Ast.Common (Name)
import Wasp.Psl.Ast.Model (Body)

data Type = Type
  { _name :: Name,
    _body :: Body,
    _attachedComments :: [AttachedComment]
  }
  deriving (Show, Eq)
