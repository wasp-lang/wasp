{{={= =}=}}
import express from 'express'
import * as crud from '../../crud/{= crud.name =}.js'
import { withSuperJsonSerialization } from '../serialization.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
{=/ isAuthEnabled =}

const _waspRouter = express.Router()

{=# crud.operations.Get =}
_waspRouter.post(
    '/{= route =}',
    {=# isAuthEnabled =}auth,{=/ isAuthEnabled =}
    withSuperJsonSerialization(crud.getFn),
)
{=/ crud.operations.Get =}
{=# crud.operations.GetAll =}
_waspRouter.post(
    '/{= route =}',
    {=# isAuthEnabled =}auth,{=/ isAuthEnabled =}
    withSuperJsonSerialization(crud.getAllFn),
)
{=/ crud.operations.GetAll =}
{=# crud.operations.Create =}
_waspRouter.post(
    '/{= route =}',
    {=# isAuthEnabled =}auth,{=/ isAuthEnabled =}
    withSuperJsonSerialization(crud.createFn),
)
{=/ crud.operations.Create =}
{=# crud.operations.Update =}
_waspRouter.post(
    '/{= route =}',
    {=# isAuthEnabled =}auth,{=/ isAuthEnabled =}
    withSuperJsonSerialization(crud.updateFn),
)
{=/ crud.operations.Update =}
{=# crud.operations.Delete =}
_waspRouter.post(
    '/{= route =}',
    {=# isAuthEnabled =}auth,{=/ isAuthEnabled =}
    withSuperJsonSerialization(crud.deleteFn),
)
{=/ crud.operations.Delete =}

export const {= crud.name =} = _waspRouter
