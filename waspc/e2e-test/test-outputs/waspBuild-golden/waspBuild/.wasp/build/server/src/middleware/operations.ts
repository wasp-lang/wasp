import { 
    deserialize as superjsonDeserialize,
    serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from 'wasp/server/utils'
import { makeAuthUserIfPossible } from 'wasp/auth/user'

export function createOperation (handlerFn) {
    return handleRejection(async (req, res) => {
        const args = (req.body && superjsonDeserialize(req.body)) || {}
        const context = {
        }  
        const result = await handlerFn(args, context)
        const serializedResult = superjsonSerialize(result)
        res.json(serializedResult)
    })
}

export function createQuery(handlerFn) {
    return createOperation(handlerFn)
}

export function createAction(handlerFn) {
    return createOperation(handlerFn)
}
