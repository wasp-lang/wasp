import express from 'express'
import * as crud from '../../crud/tasks.js'
import { withOperationsMiddleware } from '../../middleware/operations.js'
import auth from '../../core/auth.js'

const _waspRouter = express.Router()

_waspRouter.use(auth)

_waspRouter.post(
    '/get',
    withOperationsMiddleware(crud.getFn),
)
_waspRouter.post(
    '/get-all',
    withOperationsMiddleware(crud.getAllFn),
)
_waspRouter.post(
    '/create',
    withOperationsMiddleware(crud.createFn),
)

export const tasks = _waspRouter
