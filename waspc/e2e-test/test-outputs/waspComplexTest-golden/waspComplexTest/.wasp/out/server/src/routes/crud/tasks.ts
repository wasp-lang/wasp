import express from 'express'
import { 
  deserialize as superjsonDeserialize,
  serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../../utils.js'
import dbClient from '../../dbClient.js'
import HttpError from '../../core/HttpError.js'

const router = express.Router()

const entity = dbClient.task

router.post('/get', withSuperJsonSerialization((args, req) => {
    throwIfNotAuthenticated(req)
    const primaryField = args.id
    return entity.findUnique({ where: { id: primaryField } })
}))
router.post('/getAll', withSuperJsonSerialization((_args, req) => {
    return entity.findMany({})
}))
router.post('/create', withSuperJsonSerialization((args, req) => {
    throwIfNotAuthenticated(req)
    return entity.create({ data: args })
}))
router.post('/update', withSuperJsonSerialization((args, req) => {
    throwIfNotAuthenticated(req)
    const { id: primaryField, ...rest } = args
    return entity.update({ where: { id: primaryField }, data: rest })
}))

export const tasks = router

function withSuperJsonSerialization (crudFn) {
  return handleRejection(async (req, res) => {
    const args = (req.body && superjsonDeserialize(req.body)) || {}
    const result = await crudFn(args, req, res)
    const serializedResult = superjsonSerialize(result)
    res.json(serializedResult)
  })
}

function throwIfNotAuthenticated (req) {
  if (!req.user) {
    throw new HttpError(401)
  }
}
