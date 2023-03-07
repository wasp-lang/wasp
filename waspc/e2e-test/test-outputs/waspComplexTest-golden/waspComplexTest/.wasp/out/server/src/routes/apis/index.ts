import express, { Request, Response } from 'express';
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import auth from '../../core/auth.js'
import { User } from '../../entities'

import { fooBar } from '../../ext-src/apis.js'

const router = express.Router()

router.get('/foo/bar', auth, handleRejection((req: Request & { user: User }, res: Response) => {
  const context = {
    user: req.user,
    entities: {
    },
  }
  return fooBar(req, res, context)
}))

export default router
