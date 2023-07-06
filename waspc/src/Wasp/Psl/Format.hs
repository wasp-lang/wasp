module Wasp.Psl.Format () where

import Data.Text (Text)
import Wasp.Psl.Ast.Model (Model)

-- | For given prisma schema source, returns formatted schema + any warnings/errors,
-- by calling "prisma format" in the background.
-- "prisma format" does more than just formatting -> it also applies some obvious fixes,
-- like missing relationship fields. So it is kind of like compiling + formatting + fixing.
prismaFormat :: Text -> IO (Maybe String, Text)
prismaFormat prismaSchema = error "TODO"

-- "prisma format" works even without other declarations but models! That is great.

prismaFormatModels :: [Model] -> IO (Maybe String, [Model])
prismaFormatModels models = error "TODO: implement via prismaFormat"
