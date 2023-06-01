import express from 'express'
import * as crud from '../../crud/tasks.js'
import { withSuperJsonSerialization } from '../serialization.js'
import auth from '../../core/auth.js'

const _waspRouter = express.Router()

_waspRouter.post(
    '/get',
    auth,
    withSuperJsonSerialization(crud.getFn),
)
_waspRouter.post(
    '/get-all',
    auth,
    withSuperJsonSerialization(crud.getAllFn),
)
_waspRouter.post(
    '/create',
    auth,
    withSuperJsonSerialization(crud.createFn),
)

export const tasks = _waspRouter
