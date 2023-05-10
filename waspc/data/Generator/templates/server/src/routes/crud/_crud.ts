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

const entity = dbClient.{= crud.entityLower =}

{=# crud.operations.Get =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    const primaryField = args.{= crud.primaryFieldName =}
    return entity.findUnique({ where: { {= crud.primaryFieldName =}: primaryField } })
}))
{=/ isEnabled =}
{=/ crud.operations.Get =}
{=# crud.operations.GetAll =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((_args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    return entity.findMany({})
}))
{=/ isEnabled =}
{=/ crud.operations.GetAll =}
{=# crud.operations.Create =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    return entity.create({ data: args })
}))
{=/ isEnabled =}
{=/ crud.operations.Create =}
{=# crud.operations.Update =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    const { {= crud.primaryFieldName =}: primaryField, ...rest } = args.{= crud.primaryFieldName =}
    return entity.update({ where: { {= crud.primaryFieldName =}: primaryField }, data: rest })
}))
{=/ isEnabled =}
{=/ crud.operations.Update =}
{=# crud.operations.Delete =}
{=# isEnabled =}
router.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    const primaryField = args.{= crud.primaryFieldName =}
    return entity.delete({ where: { {= crud.primaryFieldName =}: primaryField } })
}))
{=/ isEnabled =}
{=/ crud.operations.Delete =}

export const {= crud.name =} = router

function withSuperJsonSerialization (crudFn) {
  return handleRejection(async (req, res) => {
    const args = (req.body && superjsonDeserialize(req.body)) || {}
    const result = await crudFn(args, req, res)
    const serializedResult = superjsonSerialize(result)
    res.json(serializedResult)
  })
}

function throwIfNotAuthenticated (req) {
  {=# isAuthEnabled =}
  if (!req.user) {
    throw new HttpError(401)
  }
  {=/ isAuthEnabled =}
  {=^ isAuthEnabled =}
  // Auth is not enabled
  {=/ isAuthEnabled =}
}
