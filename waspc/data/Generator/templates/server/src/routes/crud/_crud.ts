{{={= =}=}}
import express from 'express'
import { 
  deserialize as superjsonDeserialize,
  serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../../utils.js'
import dbClient from '../../dbClient.js'

const router = express.Router()

const entity = dbClient.{= entityLower =}

// TODO: id could be a number, string, or uuid, how to handle that?
{=# enabledOperations.Get =}
{=# operationsData.Get =}
router.post('/{= path =}', withSuperJsonSerialization((args, req, res) => {
  const id = args.id
  return entity.findUnique({ where: { id: args.id } })
}))
{=/ operationsData.Get =}
{=/ enabledOperations.Get =}

{=# enabledOperations.GetAll =}
{=# operationsData.GetAll =}
router.post('/{= path =}', withSuperJsonSerialization((args) => {
    return entity.findMany({})
}))
{=/ operationsData.GetAll =}
{=/ enabledOperations.GetAll =}

{=# enabledOperations.Create =}
{=# operationsData.Create =}
router.post('/{= path =}', withSuperJsonSerialization((args) => {
    return entity.create({ data: args })
}))
{=/ operationsData.Create =}
{=/ enabledOperations.Create =}

{=# enabledOperations.Update =}
{=# operationsData.Update =}
router.post('/{= path =}', withSuperJsonSerialization((args, req) => {
    const id = args.id
    return entity.update({ where: { id: args.id }, data: args })
}))
{=/ operationsData.Update =}
{=/ enabledOperations.Update =}

{=# enabledOperations.Delete =}
{=# operationsData.Delete =}
router.post('/{= path =}', withSuperJsonSerialization((args, req) => {
    const id = args.id
    return entity.delete({ where: { id: args.id } })
}))
{=/ operationsData.Delete =}
{=/ enabledOperations.Delete =}

export const {= name =} = router

function withSuperJsonSerialization (crudFn) {
  return handleRejection(async (req, res) => {
    const args = (req.body && superjsonDeserialize(req.body)) || {}
    const result = await crudFn(args, req, res)
    const serializedResult = superjsonSerialize(result)
    res.json(serializedResult)
  })
}
