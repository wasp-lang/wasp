import express from 'express'
import * as crud from '../../crud/taskVotes.js'
import { createAction, createQuery } from '../../middleware/operations.js'
import auth from 'wasp/server/core/auth'

const _waspRouter = express.Router()

_waspRouter.use(auth)

_waspRouter.post(
    '/get-all',
    createQuery(crud.getAllFn),
)

export const taskVotes = _waspRouter
