import express from 'express'
import * as crud from '../../crud/tasks.js'
import { withSuperJsonSerialization } from '../serialization.js'
import auth from '../../core/auth.js'

const _waspRouter = express.Router()

_waspRouter.use(auth)

_waspRouter.post(
    '/get',
    withSuperJsonSerialization(crud.getFn),
)
_waspRouter.post(
    '/get-all',
    withSuperJsonSerialization(crud.getAllFn),
)
_waspRouter.post(
    '/create',
    withSuperJsonSerialization(crud.createFn),
)

export const tasks = _waspRouter
