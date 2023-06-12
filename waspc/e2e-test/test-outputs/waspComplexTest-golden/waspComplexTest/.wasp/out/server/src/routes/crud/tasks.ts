import express from 'express'
import * as crud from '../../crud/tasks.js'
import { createAction, createQuery } from '../../middleware/operations.js'
import auth from '../../core/auth.js'

const _waspRouter = express.Router()

_waspRouter.use(auth)

_waspRouter.post(
    '/get',
    createQuery(crud.getFn),
)
_waspRouter.post(
    '/get-all',
    createQuery(crud.getAllFn),
)
_waspRouter.post(
    '/create',
    createAction(crud.createFn),
)

export const tasks = _waspRouter
