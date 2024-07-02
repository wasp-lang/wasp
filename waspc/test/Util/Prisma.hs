module Util.Prisma where

import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.Schema as Psl.Schema.Parser

getPrismaSchema :: T.Text -> Psl.Schema.Schema
getPrismaSchema = fromRight (error "Failed to parse Prisma schema") . Psl.Schema.Parser.parsePrismaSchema . T.unpack
