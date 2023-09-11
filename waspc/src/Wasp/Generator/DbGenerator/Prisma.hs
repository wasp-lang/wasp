module Wasp.Generator.DbGenerator.Prisma where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import qualified Wasp.AppSpec.App.Db as AS.Db

-- We want to show db extensions in the following format:
-- [first_ext, some_other(map: "name", schema: "schema", version: "1.0.0")]
showDbExtensions :: [AS.Db.PrismaDbExtension] -> String
showDbExtensions extensions = "[" <> intercalate ", " (map showDbExtension extensions) <> "]"
  where
    showDbExtension :: AS.Db.PrismaDbExtension -> String
    showDbExtension
      AS.Db.PrismaDbExtension
        { AS.Db.name = name,
          AS.Db.version = version,
          AS.Db.map = extensionMap,
          AS.Db.schema = schema
        } = if null fields then name else name <> "(" <> fields <> ")"
        where
          fields = unwords $ mapMaybe showField possibleFields

          showField :: (String, Maybe String) -> Maybe String
          showField (fieldName, maybeFieldValue) = maybeFieldValue <&> \fieldValue -> fieldName <> ": " <> show fieldValue

          possibleFields =
            [ ("map", extensionMap),
              ("schema", schema),
              ("version", version)
            ]
