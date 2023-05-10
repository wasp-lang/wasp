{{={= =}=}}
import express from 'express'
import auth from '../../core/auth.js'

{=# crudRouters =}
{=& importStatement =}
{=/ crudRouters =}

export const rootCrudRouter = express.Router()

{=# crudRouters =}
rootCrudRouter.use('/{= route =}', auth, {= importIdentifier =})
{=/ crudRouters =}
