import { 
    deserialize as superjsonDeserialize,
    serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../utils.js'

export function withSuperJsonSerialization (crudFn) {
    return handleRejection(async (req, res) => {
        const args = (req.body && superjsonDeserialize(req.body)) || {}
        const context = {
            user: req.user
        }  
        const result = await crudFn(args, context)
        const serializedResult = superjsonSerialize(result)
        res.json(serializedResult)
    })
}
  