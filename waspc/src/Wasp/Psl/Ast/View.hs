module Wasp.Psl.Ast.View
  ( View (..),
  )
where

import Wasp.Psl.Ast.AttachedComment (AttachedComment)
import Wasp.Psl.Ast.Common (Name)
import Wasp.Psl.Ast.Model (Body)

data View = View
  { _name :: Name,
    _body :: Body,
    _attachedComments :: [AttachedComment]
  }
  deriving (Show, Eq)
