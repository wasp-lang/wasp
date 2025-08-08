module Wasp.Psl.Valid
  ( validatePrismaSchema,
    getValidDbSystemFromPrismaSchema,
  )
where

import qualified Wasp.AppSpec.App.Db as AS
import Wasp.Project.Db
  ( DbSystemParseError (..),
    getDbSystemFromPrismaSchema,
    isDbUrlInPrismaSchemaValid,
    validDbUrlInPrismaSchema,
  )
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Psl.Util (findPrismaConfigBlockKeyValuePair)
import Wasp.Valid (ValidationError (..))
import qualified Wasp.Valid as Valid

-- Wasp expects the Prisma schema to be written in certain way:

-- * The DB providers that Wasp supports are PostgreSQL and SQLite.

-- * The DB url must be provided via the DATABASE_URL environment variable.

-- Also, there are Prisma specific requirements:

-- * The Prisma schema must have exactly one datasource block.

-- And some more Wasp specific requirements:

-- * The Prisma schema must have at least one generator block with the provider set to "prisma-client-js" (Wasp limitation).

-- The `validatePrismaSchema` makes sure that Prisma schema meets these requirements.
-- If it doesn't, it returns a list of validation errors.

validatePrismaSchema :: Psl.Schema.Schema -> [Valid.ValidationError]
validatePrismaSchema schema =
  concat
    [ -- Validating the DB related fields only if there are no other datasource validation errors,
      -- to avoid showing DB related errors if there are other more important errors.
      if null datasourceValidationErrors
        then
          concat
            [ validateDbProvider schema,
              validateDbUrl schema
            ]
        else datasourceValidationErrors,
      validateGenerators schema
    ]
  where
    datasourceValidationErrors = validateDatasources schema

validateGenerators :: Psl.Schema.Schema -> [ValidationError]
validateGenerators = validate . map Psl.WithCtx.getNode . Psl.Schema.getGenerators
  where
    validate :: [Psl.ConfigBlock.ConfigBlock] -> [ValidationError]
    validate [] = [GenericValidationError "Prisma schema should have at least one generator defined."]
    validate generators' =
      if not isTherePrismaClientJsGenerator
        then [GenericValidationError "Prisma schema should have at least one generator with the provider set to \"prisma-client-js\"."]
        else []
      where
        isTherePrismaClientJsGenerator =
          any
            ( \(Psl.ConfigBlock.ConfigBlock _type _name keyValues) ->
                findPrismaConfigBlockKeyValuePair "provider" keyValues == Just (Psl.Argument.StringExpr "prisma-client-js")
            )
            generators'

validateDatasources :: Psl.Schema.Schema -> [ValidationError]
validateDatasources = validate . map Psl.WithCtx.getNode . Psl.Schema.getDatasources
  where
    -- As per Prisma's docs there can be only ONE datasource block in the schema.
    -- https://www.prisma.io/docs/orm/reference/prisma-schema-reference#remarks
    validate :: [Psl.ConfigBlock.ConfigBlock] -> [ValidationError]
    validate [_anyDataSource] = []
    validate _ = [GenericValidationError "Prisma schema must have exactly one datasource defined."]

validateDbProvider :: Psl.Schema.Schema -> [ValidationError]
validateDbProvider = validate . getDbSystemFromPrismaSchema
  where
    validate :: Either DbSystemParseError a -> [ValidationError]
    validate (Right _dbSystem) = []
    validate (Left MissingDbSystem) =
      [ GenericValidationError
          "You need to specify the \"provider\" field in the \"datasource\" block in your Prisma schema."
      ]
    validate
      (Left (UnsupportedDbSystem unsupportedDbSystem)) =
        [ GenericValidationError $
            "Wasp doesn't support the database provider "
              ++ unsupportedDbSystem
              ++ " specified in the schema.prisma file."
        ]

validateDbUrl :: Psl.Schema.Schema -> [ValidationError]
validateDbUrl schema =
  if isDbUrlInPrismaSchemaValid schema
    then []
    else [GenericValidationError $ "The \"url\" field in the \"datasource\" block in your Prisma schema must be set to " ++ validDbUrlInPrismaSchema ++ "."]

-- | Returns the database system specified in the Prisma schema file.
-- Using this function assumes that the Prisma schema file has
-- been validated with the `validatePrismaSchema` function.
getValidDbSystemFromPrismaSchema :: Psl.Schema.Schema -> AS.DbSystem
getValidDbSystemFromPrismaSchema schema =
  case getDbSystemFromPrismaSchema schema of
    Right dbSystem -> dbSystem
    Left _ -> error "Unable to get valid DB system from Prisma schema. This is probably a bug in the Wasp compiler, please report it."
