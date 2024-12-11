{{={= =}=}}
import { prisma } from 'wasp/server'

{=# isAuthEnabled =}
import { createInvalidCredentialsError } from 'wasp/auth/utils'
{=/ isAuthEnabled =}
import type {
  {=# crud.operations.GetAll =}
  GetAllQueryResolved,
  {=/ crud.operations.GetAll =}
  {=# crud.operations.Get =}
  GetQueryResolved,
  {=/ crud.operations.Get =}
  {=# crud.operations.Create =}
  CreateActionResolved,
  {=/ crud.operations.Create =}
  {=# crud.operations.Update =}
  UpdateActionResolved,
  {=/ crud.operations.Update =}
  {=# crud.operations.Delete =}
  DeleteActionResolved,
  {=/ crud.operations.Delete =}
} from 'wasp/server/crud/{= crud.name =}'
{=# overrides.GetAll.isDefined =}
{=& overrides.GetAll.importStatement =}
{=/ overrides.GetAll.isDefined =}
{=# overrides.Get.isDefined =}
{=& overrides.Get.importStatement =}
{=/ overrides.Get.isDefined =}
{=# overrides.Create.isDefined =}
{=& overrides.Create.importStatement =}
{=/ overrides.Create.isDefined =}
{=# overrides.Update.isDefined =}
{=& overrides.Update.importStatement =}
{=/ overrides.Update.isDefined =}
{=# overrides.Delete.isDefined =}
{=& overrides.Delete.importStatement =}
{=/ overrides.Delete.isDefined =}

const entities = {
  {= crud.entityUpper =}: prisma.{= crud.entityLower =},
}

{=!
// Let's explain this template on the GetAll operation example
=}
{=# crud.operations.GetAll =}
// Get All query
{=!
// 1. We either use the default implementation of the operation...
=}
{=^ overrides.GetAll.isDefined =}
const _waspGetAllQuery: GetAllQueryResolved = ((args, context) => {
  {=^ crud.operations.GetAll.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.GetAll.isPublic =}
  return context.entities.{= crud.entityUpper =}.findMany();
});
{=/ overrides.GetAll.isDefined =}
{=!
// ... or the one defined in the overrides by the user. We use the "importIdentifier" property to
// reference the function from the overrides.
=}
{=# overrides.GetAll.isDefined =}
const _waspGetAllQuery = {= overrides.GetAll.importIdentifier =}
{=/ overrides.GetAll.isDefined =}

{=!
// 2. We define a function that is used as the Express route handler
=}
export async function getAllFn(args, context) {
  return (_waspGetAllQuery as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.GetAll =}
{=!
// That's it! It is similar for all other operations.
=}  

{=# crud.operations.Get =}
// Get query
{=^ overrides.Get.isDefined =}
const _waspGetQuery: GetQueryResolved = ((args, context) => {
  {=^ crud.operations.Get.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Get.isPublic =}
  return context.entities.{= crud.entityUpper =}.findUnique({ where: { id: args.id } });
});
{=/ overrides.Get.isDefined =}
{=# overrides.Get.isDefined =}
const _waspGetQuery = {= overrides.Get.importIdentifier =}
{=/ overrides.Get.isDefined =}

export async function getFn(args, context) {
  return (_waspGetQuery as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Get =}

{=# crud.operations.Create =}
// Create action
{=^ overrides.Create.isDefined =}
const _waspCreateAction: CreateActionResolved = ((args, context) => {
  {=^ crud.operations.Create.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Create.isPublic =}
  return context.entities.{= crud.entityUpper =}.create({ data: args });
});
{=/ overrides.Create.isDefined =}
{=# overrides.Create.isDefined =}
const _waspCreateAction = {= overrides.Create.importIdentifier =}
{=/ overrides.Create.isDefined =}

export async function createFn(args, context) {
  return (_waspCreateAction as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Create =}

{=# crud.operations.Update =}
// Update action
{=^ overrides.Update.isDefined =}
const _waspUpdateAction: UpdateActionResolved = ((args, context) => {
  {=^ crud.operations.Update.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Update.isPublic =}
  const { {= crud.idFieldName =}: idFieldValue, ...rest } = args
  return context.entities.{= crud.entityUpper =}.update({
    where: { {= crud.idFieldName =}: idFieldValue },
    data: rest,
  });
});
{=/ overrides.Update.isDefined =}
{=# overrides.Update.isDefined =}
const _waspUpdateAction = {= overrides.Update.importIdentifier =}
{=/ overrides.Update.isDefined =}

export async function updateFn(args, context) {
  return (_waspUpdateAction as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Update =}

{=# crud.operations.Delete =}
// Delete action
{=^ overrides.Delete.isDefined =}
const _waspDeleteAction: DeleteActionResolved = ((args, context) => {
  {=^ crud.operations.Delete.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Delete.isPublic =}
  const { {= crud.idFieldName =}: idFieldValue } = args
  return context.entities.{= crud.entityUpper =}.delete({ where:  { {= crud.idFieldName =}: idFieldValue } });
});
{=/ overrides.Delete.isDefined =}
{=# overrides.Delete.isDefined =}
const _waspDeleteAction = {= overrides.Delete.importIdentifier =}
{=/ overrides.Delete.isDefined =}

export async function deleteFn(args, context) {
  return (_waspDeleteAction as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Delete =}

function throwIfNotAuthenticated (context) {
  {=# isAuthEnabled =}
  if (!context.user) {
    throw createInvalidCredentialsError()
  }
  {=/ isAuthEnabled =}
  {=^ isAuthEnabled =}
  // Auth is not enabled
  {=/ isAuthEnabled =}
}