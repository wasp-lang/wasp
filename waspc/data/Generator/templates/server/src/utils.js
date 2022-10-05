import Prisma from '@prisma/client'
import HttpError from './core/HttpError.js'

/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns {Func} Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
export const handleRejection = (middleware) => async (req, res, next) => {
  try {
    await middleware(req, res, next)
  } catch (error) {
    next(error)
  }
}

export const isPrismaError = (e) => {
  return e instanceof Prisma.PrismaClientKnownRequestError ||
    e instanceof Prisma.PrismaClientUnknownRequestError ||
    e instanceof Prisma.PrismaClientRustPanicError ||
    e instanceof Prisma.PrismaClientInitializationError ||
    e instanceof Prisma.PrismaClientValidationError
}

export const prismaErrorToHttpError = (e) => {
  if (e instanceof Prisma.PrismaClientKnownRequestError) {

    switch (e.code) {
      case 'P1000':
        return new HttpError(511, 'Correct Authentication Required', {message: "Authentication failed against database server, the provided database credentials are not valid. Please make sure to provide valid database credentials for the database server."})
      case 'P1001':
        return new HttpError(504, 'Database Timeout', {message: "Can't reach database server. Please make sure your database server is running."})
      case 'P1002':
        return new HttpError(504, 'Database Timeout', {message: 'The database server was reached but timed out. Please try again. Please make sure your database server is running.'})
      case 'P1003':
        return new HttpError(404, 'Not Found', {message: 'Database name, file, or column does not exist.'})
      case 'P1008':
        return new HttpError(504, 'Operations Timeout', {message: 'Operations timed out.'})
      case 'P1009':
        return new HttpError(500, 'Duplicated Database', {message: 'Database already exists on the database server.'})
      case 'P1010':
        return new HttpError(511, 'Database User Error', {message: 'User was denied access on the database.'})
      case 'P1011':
        return new HttpError(500, 'Security Implementation Error', {message: 'Error opening a TLS connection.'})
      case 'P1012':
        return new HttpError(500, 'Update Error', {message: 'Invalid version after update.'})
      case 'P1013':
        return new HttpError(500, 'Database Error', {message: 'The provided database string is invalid.'})
      case 'P1014':
        return new HttpError(404, 'Not Found', {message: 'This model does not exist.'})
      case 'P1015':
        return new HttpError(405, 'Feature Not Allowed', {message: 'Your schema is using features that are not supported for the version of the database.'})
      case 'P1016':
        return new HttpError(416, 'Expect Correct Parameters', {message: 'Your raw query had an incorrect number of parameters.'})
      case 'P1017':
        return new HttpError(408, 'Server Timeout', {message: 'Server has closed the connection.'})
      case 'P2000':
        return new HttpError(413, 'Value Too Large', {message: "The provided value for the column is too long for the column's type."})
      case 'P2001':
        return new HttpError(404, 'Not Found', {message: 'The record searched does not exist.'})
      case 'P2002':
        return new HttpError(422, 'Save failed', {
          message: `A record with the same ${e.meta.target.join(', ')} already exists.`,
          target: e.meta.target
        })
      case 'P2003':
        return new HttpError(500, 'Bad Foreing Key', {message: 'Foreign key constraint failed.'})
      case 'P2004':
        return new HttpError(500, 'Database Error', {message: 'A constraint failed on the database.'})
      case 'P2005':
        return new HttpError(400, 'Bad Type', {message: "The value stored in the database for the field is invalid for the field's type"})
      case 'P2006':
        return new HttpError(400, 'Invalid Value', {message: 'The provided value for field is not valid'})
      case 'P2007':
        return new HttpError(500, 'Data Validation Error', {message: 'Data validation error'})
      case 'P2008':
        return new HttpError(400, 'Bad Query', {message: 'Failed to parse the query'})
      case 'P2009':
        return new HttpError(500, 'Bad Query', {message: 'Failed to validate the query'})
      case 'P2010':
        return new HttpError(400, 'Bad Query', {message: 'Raw query failed.'})
      case 'P2011':
        return new HttpError(400, 'Constraint Violation', {message: 'Null constraint violation.'})
      case 'P2012':
        return new HttpError(400, 'Missing Value', {message: 'Missing a required value.'})
      case 'P2013':
        return new HttpError(400, 'Missing Argument', {message: 'Missing the required argument.'})
      case 'P2014':
        return new HttpError(400, 'Bad Change', {message: 'The change you are trying to make would violate the required relation.'})
      case 'P2015':
        return new HttpError(404, 'Record Not Found', {message: 'A related record could not be found.'})
      case 'P2016':
        return new HttpError(500, 'Interpretation Error', {message: 'Query interpretation error.'})
      case 'P2017':
        return new HttpError(400, 'Bad Change', {message: 'The records for the relation models are not connected.'})
      case 'P2018':
        return new HttpError(404, 'Record Not Found', {message: 'The required connected records were not found.'})
      case 'P2019':
        return new HttpError(400, 'Input Error', {message: 'Input error.'})
      case 'P2020':
        return new HttpError(413, 'Value Out Of Range', {message: 'Value out of range for the type.'})
      case 'P2021':
        return new HttpError(404, 'Table Not Found', {message: 'The table does not exist in the current database.'})
      case 'P2022':
        return new HttpError(404, 'Column Not Found', {message: 'The column does not exist in the current database.'})
      case 'P2023':
        return new HttpError(400, 'Inconsistent Data', {message: 'Inconsistent column data.'})
      case 'P2024':
        return new HttpError(408, 'Request Timeout', {message: 'Timed out fetching a new connection from the connection pool.'})
      case 'P2025':
        return new HttpError(404, 'Record Not Found', {message: 'An operation failed because it depends on one or more records that were required but not found.'})
      case 'P2026':
        return new HttpError(405, 'Feature Not Allowed', {message: "The current database provider doesn't support a feature that the query used."})
      case 'P2027':
        return new HttpError(500, 'Error', {message: "Multiple errors occurred on the database during query execution."})
      case 'P2028':
        return new HttpError(400, 'Transaction Error', {message: "Transaction API error."})
      case 'P2030':
        return new HttpError(500, 'Schema Error', {message: "Cannot find a fulltext index to use for the search, try adding a @@fulltext([Fields...]) to your schema."})
      case 'P2031':
        return new HttpError(500, 'MongoDB Error', {message: "Prisma needs to perform transactions, which requires your MongoDB server to be run as a replica set. See details: https://pris.ly/d/mongodb-replica-set"})
      case 'P2033':
        return new HttpError(400, 'Bad Type', {message: "A number used in the query does not fit into a 64 bit signed integer. Consider using BigInt as field type if you're trying to store large integers"})
      case 'P2034':
        return new HttpError(500, 'Transaction Failed', {message: 'Transaction failed due to a write conflict or a deadlock. Please retry your transaction'})
      case 'P3000':
        return new HttpError(500, 'Error', {message: 'Failed to create database'})
      case 'P3001':
        return new HttpError(500, 'Error', {message: 'Migration possible with destructive changes and possible data loss'})
      case 'P3002':
        return new HttpError(500, 'Error', {message: 'The attempted migration was rolled back'})
      case 'P3003':
        return new HttpError(500, 'Error', {message: 'The format of migrations changed, the saved migrations are no longer valid. To solve this problem, please follow the steps at: https://pris.ly/d/migrate'})
      case 'P3004':
        return new HttpError(500, 'Error', {message: 'This database is a system database, it should not be altered with prisma migrate. Please connect to another database.'})
      case 'P3005':
        return new HttpError(500, 'Error', {message: 'The database schema is not empty. Read more about how to baseline an existing production database: https://pris.ly/d/migrate-baseline'})
      case 'P3006':
        return new HttpError(500, 'Migration Failed', {message: 'Migration failed to apply cleanly to the shadow database.'})
      case 'P3007':
        return new HttpError(500, 'Error', {message: 'Some of the requested preview features are not yet allowed in migration engine. Please remove them from your data model before using migrations.'})
      case 'P3008':
        return new HttpError(500, 'Error', {message: 'The migration is already recorded as applied in the database.'})
      case 'P3009':
        return new HttpError(500, 'Error', {message: 'Migrate found failed migrations in the target database, new migrations will not be applied. Read more about how to resolve migration issues in a production database: https://pris.ly/d/migrate-resolve'})
      case 'P3010':
        return new HttpError(500, 'Migration Failed', {message: 'The name of the migration is too long. It must not be longer than 200 characters'})
      case 'P3011':
        return new HttpError(500, 'Error', {message: 'Migration cannot be rolled back because it was never applied to the database. Hint: did you pass in the whole migration name? (example: \"20201207184859_initial_migration\")'})
      case 'P3012':
        return new HttpError(500, 'Error', {message: 'Migration cannot be rolled back because it is not in a failed state.'})
      case 'P3013':
        return new HttpError(500, 'Error', {message: 'Datasource provider arrays are no longer supported in migrate. Please change your datasource to use a single provider. Read more at https://pris.ly/multi-provider-deprecation'})
      case 'P3014':
        return new HttpError(500, 'Error', {message: 'Prisma Migrate could not create the shadow database. Please make sure the database user has permission to create databases. More info: https://pris.ly/d/migrate-shadow.'})
      case 'P3015':
        return new HttpError(500, 'Missing File', {message: 'Could not find the migration file. Please delete the directory or restore the migration file.'})
      case 'P3016':
        return new HttpError(500, 'Error', {message: 'The fallback method for database resets failed, meaning Migrate could not clean up the database entirely.'})
      case 'P3017':
        return new HttpError(500, 'Missing Migration', {message: 'The migration could not be found. Please make sure that the migration exists, and that you included the whole name of the directory. (example: \"20201207184859_initial_migration\")'})
      case 'P3018':
        return new HttpError(500, 'Error', {message: 'A migration failed to apply. New migrations can not be applied before the error is recovered from. Read more about how to resolve migration issues in a production database: https://pris.ly/d/migrate-resolve'})
      case 'P3019':
        return new HttpError(500, 'Error', {message: 'The datasource provider specified in your schema does not match the one specified in the migration_lock.toml. Please remove your current migration directory and start a new migration history with prisma migrate dev. Read more: https://pris.ly/d/migrate-provider-switch'})
      case 'P3020':
        return new HttpError(500, 'Error', {message: 'The automatic creation of shadow databases is disabled on Azure SQL. Please set up a shadow database using the shadowDatabaseUrl datasource attribute. Read the docs page for more details: https://pris.ly/d/migrate-shadow'})
      case 'P3021':
        return new HttpError(500, 'Error', {message: 'Foreign keys cannot be created on this database. Learn more how to handle this: https://pris.ly/d/migrate-no-foreign-keys'})
      case 'P3022':
        return new HttpError(500, 'Error', {message: 'Direct execution of DDL (Data Definition Language) SQL statements is disabled on this database. Please read more here about how to handle this: https://pris.ly/d/migrate-no-direct-ddl'})
      case 'P4000':
        return new HttpError(500, 'Error', {message: 'Introspection operation failed to produce a schema file'})
      case 'P4001':
        return new HttpError(500, 'Error', {message: 'The introspected database was empty'})
      case 'P4002':
        return new HttpError(500, 'Error', {message: 'The schema of the introspected database was inconsistent'})
      default:
        return new HttpError(500)
    }
  } else if (e instanceof Prisma.PrismaClientValidationError) {
    return new HttpError(422, 'Save failed')
  } else {
    return new HttpError(500)
  }
}

export const sleep = ms => new Promise(r => setTimeout(r, ms))
