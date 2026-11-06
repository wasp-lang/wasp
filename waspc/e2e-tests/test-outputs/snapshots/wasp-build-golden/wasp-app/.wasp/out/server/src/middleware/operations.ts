import { deserialize, serialize } from 'wasp/core/serialization'
import { defineHandler } from 'wasp/server/utils'

export function createOperation (handlerFn) {
    return defineHandler(async (req, res) => {
        const payload = req.body && deserialize(req.body)
        // We only default when no payload was sent, since falsy values like
        // `false` or `0` are valid payloads.
        const args = payload === undefined ? {} : payload
        const context = {
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
