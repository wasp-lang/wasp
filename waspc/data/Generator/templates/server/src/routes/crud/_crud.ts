{{={= =}=}}
import express from 'express'
import { 
  deserialize as superjsonDeserialize,
  serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../../utils.js'
import dbClient from '../../dbClient.js'
import HttpError from '../../core/HttpError.js'

const router = express.Router()

const entity = dbClient.{= entityLower =}

{=# operationsData.Get =}
{=# isEnabled =}
router.post('/{= path =}', withSuperJsonSerialization((args, req) => {
  {=^ isPublic =}
  if (!req.user) {
    throw new HttpError(401)
  }
  {=/ isPublic =}
  const primaryField = args.{= primaryFieldName =}
  return entity.findUnique({ where: { {= primaryFieldName =}: primaryField } })
}))
{=/ isEnabled =}
{=/ operationsData.Get =}
{=# operationsData.GetAll =}
{=# isEnabled =}
router.post('/{= path =}', withSuperJsonSerialization((_args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    return entity.findMany({})
}))
{=/ isEnabled =}
{=/ operationsData.GetAll =}
{=# operationsData.Create =}
{=# isEnabled =}
router.post('/{= path =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    return entity.create({ data: args })
}))
{=/ isEnabled =}
{=/ operationsData.Create =}
{=# operationsData.Update =}
{=# isEnabled =}
router.post('/{= path =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    const { {= primaryFieldName =}: primaryField, ...rest } = args.{= primaryFieldName =}
    return entity.update({ where: { {= primaryFieldName =}: primaryField }, data: rest })
}))
{=/ isEnabled =}
{=/ operationsData.Update =}
{=# operationsData.Delete =}
{=# isEnabled =}
router.post('/{= path =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    const primaryField = args.{= primaryFieldName =}
    return entity.delete({ where: { {= primaryFieldName =}: primaryField } })
}))
{=/ isEnabled =}
{=/ operationsData.Delete =}

export const {= name =} = router

function withSuperJsonSerialization (crudFn) {
  return handleRejection(async (req, res) => {
    const args = (req.body && superjsonDeserialize(req.body)) || {}
    const result = await crudFn(args, req, res)
    const serializedResult = superjsonSerialize(result)
    res.json(serializedResult)
  })
}
