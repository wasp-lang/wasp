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
