{{={= =}=}}
import express from 'express'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
{=/ isAuthEnabled =}

{=# crudRouters =}
{=& importStatement =}
{=/ crudRouters =}

export const rootCrudRouter = express.Router()

{=# crudRouters =}
rootCrudRouter.use('/{= route =}', {=# isAuthEnabled =}auth, {=/ isAuthEnabled =}{= importIdentifier =})
{=/ crudRouters =}
