{{={= =}=}}
import express from 'express'
import * as crud from '../../crud/{= crud.name =}.js'
import { createAction, createQuery } from '../../middleware/operations.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
{=/ isAuthEnabled =}

const _waspRouter = express.Router()

{=# isAuthEnabled =}
_waspRouter.use(auth)
{=/ isAuthEnabled =}

{=# crud.operations.Get =}
_waspRouter.post(
    '/{= route =}',
    createQuery(crud.getFn),
)
{=/ crud.operations.Get =}
{=# crud.operations.GetAll =}
_waspRouter.post(
    '/{= route =}',
    createQuery(crud.getAllFn),
)
{=/ crud.operations.GetAll =}
{=# crud.operations.Create =}
_waspRouter.post(
    '/{= route =}',
    createAction(crud.createFn),
)
{=/ crud.operations.Create =}
{=# crud.operations.Update =}
_waspRouter.post(
    '/{= route =}',
    createAction(crud.updateFn),
)
{=/ crud.operations.Update =}
{=# crud.operations.Delete =}
_waspRouter.post(
    '/{= route =}',
    createAction(crud.deleteFn),
)
{=/ crud.operations.Delete =}

export const {= crud.name =} = _waspRouter
