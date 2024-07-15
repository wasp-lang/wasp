module Wasp.Psl.Ast.View
  ( View (..),
  )
where

import Wasp.Psl.Ast.Common (Name)
import Wasp.Psl.Ast.Model (Body)

data View
  = View
      Name
      Body
  deriving (Show, Eq)
