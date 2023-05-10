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

{=# operations.Get =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
  {=^ isPublic =}
  if (!req.user) {
    throw new HttpError(401)
  }
  {=/ isPublic =}
  const primaryField = args.{= primaryFieldName =}
  return entity.findUnique({ where: { {= primaryFieldName =}: primaryField } })
}))
{=/ isEnabled =}
{=/ operations.Get =}
{=# operations.GetAll =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((_args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    return entity.findMany({})
}))
{=/ isEnabled =}
{=/ operations.GetAll =}
{=# operations.Create =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    return entity.create({ data: args })
}))
{=/ isEnabled =}
{=/ operations.Create =}
{=# operations.Update =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    const { {= primaryFieldName =}: primaryField, ...rest } = args.{= primaryFieldName =}
    return entity.update({ where: { {= primaryFieldName =}: primaryField }, data: rest })
}))
{=/ isEnabled =}
{=/ operations.Update =}
{=# operations.Delete =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    if (!req.user) {
      throw new HttpError(401)
    }
    {=/ isPublic =}
    const primaryField = args.{= primaryFieldName =}
    return entity.delete({ where: { {= primaryFieldName =}: primaryField } })
}))
{=/ isEnabled =}
{=/ operations.Delete =}

export const {= name =} = router

function withSuperJsonSerialization (crudFn) {
  return handleRejection(async (req, res) => {
    const args = (req.body && superjsonDeserialize(req.body)) || {}
    const result = await crudFn(args, req, res)
    const serializedResult = superjsonSerialize(result)
    res.json(serializedResult)
  })
}
