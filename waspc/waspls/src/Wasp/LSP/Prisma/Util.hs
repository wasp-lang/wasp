module Wasp.LSP.Prisma.Util where

import qualified Wasp.Psl.Ast.Schema as Psl.Ast

showModels :: Psl.Ast.Schema -> String
showModels = unwords . getModelNames

getModelNames :: Psl.Ast.Schema -> [String]
getModelNames = fmap (\(Psl.Ast.Model name _) -> name) . Psl.Ast.getModels
