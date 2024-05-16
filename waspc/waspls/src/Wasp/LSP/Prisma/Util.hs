module Wasp.LSP.Prisma.Util where

import Wasp.Analyzer (takeDecls)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Entity as AS.Entity

showEntities :: [AS.Decl] -> String
showEntities = unwords . map fst . getEntities

getEntities :: [AS.Decl] -> [(String, AS.Entity.Entity)]
getEntities = takeDecls
