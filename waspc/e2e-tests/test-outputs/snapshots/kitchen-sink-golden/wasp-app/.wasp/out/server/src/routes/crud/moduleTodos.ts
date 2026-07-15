import express from 'express'
import * as crud from '../../crud/moduleTodos.js'
import { createAction, createQuery } from '../../middleware/operations.js'
import auth from 'wasp/core/auth'

const _waspRouter = express.Router()

_waspRouter.use(auth)

_waspRouter.post(
    '/get-all',
    createQuery(crud.getAllFn),
)
_waspRouter.post(
    '/update',
    createAction(crud.updateFn),
)
_waspRouter.post(
    '/delete',
    createAction(crud.deleteFn),
)

export const moduleTodos = _waspRouter
