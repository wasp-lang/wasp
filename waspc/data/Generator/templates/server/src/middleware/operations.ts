{{={= =}=}}
import { deserialize, serialize } from 'wasp/core/serialization'
import { handleRejection } from 'wasp/server/utils'
import { makeAuthUserIfPossible } from 'wasp/auth/user'

export function createOperation (handlerFn) {
    return handleRejection(async (req, res) => {
        const args = (req.body && deserialize(req.body)) || {}
        const context = {
            {=# isAuthEnabled =}
            user: makeAuthUserIfPossible(req.user),
            {=/ isAuthEnabled =}
        }
        const result = await handlerFn(args, context)
        const serializedResult = serialize(result)
        res.json(serializedResult)
    })
}

export function createQuery(handlerFn) {
    return createOperation(handlerFn)
}

export function createAction(handlerFn) {
    return createOperation(handlerFn)
}
