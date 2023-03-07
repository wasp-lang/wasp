import express, { Request, Response } from 'express';
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'

import { fooBar } from '../../ext-src/apis.js'

const router = express.Router()

router.get('/foo/bar', handleRejection((req: Request, res: Response) => {
  const context = {
    entities: {
    },
  }
  return fooBar(req, res, context)
}))

export default router
