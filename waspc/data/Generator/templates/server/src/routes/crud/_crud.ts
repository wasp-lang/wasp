{{={= =}=}}
import express from 'express'
import { 
  deserialize as superjsonDeserialize,
  serialize as superjsonSerialize,
} from 'superjson'
import { handleRejection } from '../../utils.js'
import dbClient from '../../dbClient.js'
import HttpError from '../../core/HttpError.js'
{=# overrides.isDefined =}
{=& overrides.importStatement =}
{=/ overrides.isDefined =}

const _waspRouter = express.Router()

const _waspEntity = dbClient.{= crud.entityLower =}

{=# overrides.isDefined =}
const _waspOverrides = {= overrides.importIdentifier =}
{=/ overrides.isDefined =}
{=# crud.operations.Get =}
{=# isEnabled =}
_waspRouter.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    const primaryField = args.{= crud.primaryFieldName =}
    let query = { where: { {= crud.primaryFieldName =}: primaryField } } as any
    {=# overrides.isDefined =}
    query = _waspOverrides.Get ? { ...query, ..._waspOverrides.Get(args, req.user) } : query
    {=/ overrides.isDefined =}
    return _waspEntity.findUnique(query)
}))
{=/ isEnabled =}
{=/ crud.operations.Get =}
{=# crud.operations.GetAll =}
{=# isEnabled =}
_waspRouter.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    let query = {} as any
    {=# overrides.isDefined =}
    query = _waspOverrides.GetAll ? { ...query, ..._waspOverrides.GetAll(args, req.user) } : query
    {=/ overrides.isDefined =}
    return _waspEntity.findMany(query)
}))
{=/ isEnabled =}
{=/ crud.operations.GetAll =}
{=# crud.operations.Create =}
{=# isEnabled =}
_waspRouter.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    let query = { data: args } as any
    {=# overrides.isDefined =}
    query = _waspOverrides.Create ? { ...query, ..._waspOverrides.Create(args, req.user) } : query
    {=/ overrides.isDefined =}
    return _waspEntity.create(query)
}))
{=/ isEnabled =}
{=/ crud.operations.Create =}
{=# crud.operations.Update =}
{=# isEnabled =}
_waspRouter.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    const { {= crud.primaryFieldName =}: primaryField, ...rest } = args
    let query = { where: { {= crud.primaryFieldName =}: primaryField }, data: rest } as any
    {=# overrides.isDefined =}
    query = _waspOverrides.Update ? { ...query, ..._waspOverrides.Update(args, req.user) } : query
    {=/ overrides.isDefined =}
    return _waspEntity.update(query)
}))
{=/ isEnabled =}
{=/ crud.operations.Update =}
{=# crud.operations.Delete =}
{=# isEnabled =}
_waspRouter.post('/{= route =}', withSuperJsonSerialization((args, req) => {
    {=^ isPublic =}
    throwIfNotAuthenticated(req)
    {=/ isPublic =}
    const primaryField = args.{= crud.primaryFieldName =}
    let query = { where: { {= crud.primaryFieldName =}: primaryField } } as any
    {=# overrides.isDefined =}
    query = _waspOverrides.Delete ? { ...query, ..._waspOverrides.Delete(args, req.user) } : query
    {=/ overrides.isDefined =}
    return _waspEntity.delete(query)
}))
{=/ isEnabled =}
{=/ crud.operations.Delete =}

export const {= crud.name =} = _waspRouter

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
