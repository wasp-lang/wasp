import { Request, Response, NextFunction } from 'express'
import Prisma from '@prisma/client'

import { readdir } from 'fs'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'

import { type SanitizedUser } from './_types/index.js'

/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns {Func} Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
type RequestWithExtraFields = Request & {
  user?: SanitizedUser
}
export const handleRejection = (
  middleware: (
    req: RequestWithExtraFields,
    res: Response,
    next: NextFunction
  ) => any
) =>
async (req: RequestWithExtraFields, res: Response, next: NextFunction) => {
  try {
    await middleware(req, res, next)
  } catch (error) {
    next(error)
  }
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
