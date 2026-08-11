{{={= =}=}}
import { deserialize, serialize } from 'wasp/core/serialization'
import { defineHandler } from 'wasp/server/utils'
{=# isAuthEnabled =}
import { makeAuthUserIfPossible } from 'wasp/auth/user'
{=/ isAuthEnabled =}

type OperationHandlerFn = (args: any, context: any) => unknown

export function createOperation (handlerFn: OperationHandlerFn) {
    return defineHandler(async (req, res) => {
        const args = (req.body && deserialize(req.body)) || {}
        const context = {
            {=# isAuthEnabled =}
            user: makeAuthUserIfPossible(req.user ?? null),
            {=/ isAuthEnabled =}
        }
        const result = await handlerFn(args, context)
        const serializedResult = serialize(result)
        res.json(serializedResult)
    })
}

export function createQuery(handlerFn: OperationHandlerFn) {
    return createOperation(handlerFn)
}

export function createAction(handlerFn: OperationHandlerFn) {
    return createOperation(handlerFn)
}
