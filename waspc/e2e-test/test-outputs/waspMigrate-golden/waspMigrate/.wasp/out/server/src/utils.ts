import { Request, Response, NextFunction } from 'express'
import { Prisma } from '@prisma/client'

import { readdir } from 'fs'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'

import HttpError from './core/HttpError.js'
import { type SanitizedUser } from './_types/index.js'

/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns {Func} Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
type RequestWithUser = Request & { user?: SanitizedUser }
export const handleRejection = (
  middleware: (
    req: RequestWithUser,
    res: Response,
    next: NextFunction
  ) => Promise<unknown>
) =>
async (req: RequestWithUser, res: Response, next: NextFunction) => {
  try {
    await middleware(req, res, next)
  } catch (error) {
    next(error)
  }
}

export const isPrismaError = (e: unknown) => {
  return (
    e instanceof Prisma.PrismaClientKnownRequestError ||
    e instanceof Prisma.PrismaClientUnknownRequestError ||
    e instanceof Prisma.PrismaClientRustPanicError ||
    e instanceof Prisma.PrismaClientInitializationError ||
    e instanceof Prisma.PrismaClientValidationError
  )
}

export const prismaErrorToHttpError = (e: unknown) => {
  if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2002') {
    return new HttpError(422, 'Save failed', {
      message: `user with the same identity already exists`,
    })
  }
  if (e instanceof Prisma.PrismaClientValidationError) {
    // NOTE: Logging the error since this usually means that there are
    // required fields missing in the request.
    console.error(e)
    return new HttpError(422, 'Save failed', {
      message: 'there was a database error'
    })
  }
  return new HttpError(500)
}

export const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))

export function getDirFromFileUrl(fileUrl: string) {
  return fileURLToPath(dirname(fileUrl))
}

export async function importJsFilesFromDir(
  absoluteDir: string,
  relativePath: string,
  whitelist: string[] | null = null
): Promise<any[]> {
  const pathToDir = join(absoluteDir, relativePath)

  return new Promise((resolve, reject) => {
    readdir(pathToDir, async (err, files) => {
      if (err) {
        return reject(err)
      }
      const importPromises = files
        .filter((file) => file.endsWith('.js') && isWhitelisted(file))
        .map((file) => import(`${pathToDir}/${file}`))
      resolve(Promise.all(importPromises))
    })
  })

  function isWhitelisted(file: string) {
    // No whitelist means all files are whitelisted
    if (!Array.isArray(whitelist)) {
      return true
    }
    return whitelist.some((whitelistedFile) => file.endsWith(whitelistedFile))
  }
}
