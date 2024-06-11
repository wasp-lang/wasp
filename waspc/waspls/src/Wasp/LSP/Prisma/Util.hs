module Wasp.LSP.Prisma.Util where

import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema

showModels :: Psl.Schema.Schema -> String
showModels = unwords . getModelNames

getModelNames :: Psl.Schema.Schema -> [String]
getModelNames = fmap (\(Psl.Model.Model name _) -> name) . Psl.Schema.getModels
