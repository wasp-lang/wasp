{{={= =}=}}
import express from 'express'

{=# crudRouters =}
{=& importStatement =}
{=/ crudRouters =}

export const rootCrudRouter = express.Router()

{=# crudRouters =}
rootCrudRouter.use('{= routePath =}', {= importIdentifier =})
{=/ crudRouters =}
