module Wasp.LSP.Prisma.Util
  ( showModelNames,
  )
where

import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema

showModelNames :: Psl.Schema.Schema -> String
showModelNames = unwords . getModelNames

getModelNames :: Psl.Schema.Schema -> [String]
getModelNames = fmap (\(Psl.Model.Model name _) -> name) . Psl.Schema.getModels
