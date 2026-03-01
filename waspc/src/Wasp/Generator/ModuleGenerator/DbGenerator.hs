module Wasp.Generator.ModuleGenerator.DbGenerator
  ( genSyntheticPrismaSchema,
  )
where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (File', Path', Rel, relfile)
import qualified StrongPath as SP
import Wasp.AppSpec.Module (EntityDeclaration (..), ModuleSpec (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)

genSyntheticPrismaSchema :: ModuleSpec -> [FileDraft]
genSyntheticPrismaSchema spec =
  [ createTextFileDraft schemaFilePath (renderPrismaSchema spec)
  ]

schemaFilePath :: Path' (Rel ProjectRootDir) File'
schemaFilePath = SP.castFile [relfile|db/schema.prisma|]

renderPrismaSchema :: ModuleSpec -> Text
renderPrismaSchema spec =
  T.unlines $
    [ T.pack "// Generated from module.wasp.ts entity declarations",
      T.pack "datasource db {",
      T.pack "  provider = \"sqlite\"",
      T.pack "  url      = \"file:./module-dev.db\"",
      T.pack "}",
      T.pack "",
      T.pack "generator client {",
      T.pack "  provider = \"prisma-client-js\"",
      T.pack "}",
      T.pack ""
    ]
      ++ concatMap renderModel (msEntities spec)

renderModel :: EntityDeclaration -> [Text]
renderModel decl =
  [ T.pack $ "model " ++ edName decl ++ " {" ]
    ++ map renderField (Map.toList $ edFields decl)
    ++ [ T.pack "}", T.pack "" ]

renderField :: (String, String) -> Text
renderField (fieldName, fieldType) =
  T.pack $ "  " ++ fieldName ++ " " ++ fieldType
