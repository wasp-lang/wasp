import { 
    deserialize as superjsonDeserialize,
    serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../utils.js'

export function withOperationsMiddleware (handlerFn) {
    return handleRejection(async (req, res) => {
        const args = (req.body && superjsonDeserialize(req.body)) || {}
        const context = {
            user: req.user
        }  
        const result = await handlerFn(args, context)
        const serializedResult = superjsonSerialize(result)
        res.json(serializedResult)
    })
}
