{{={= =}=}}
import { prisma } from 'wasp/server';

import type { Prisma } from "@prisma/client";
import type {
  {= crud.entityUpper =},
} from "wasp/entities";
{=# isAuthEnabled =}
import { throwInvalidCredentialsError } from 'wasp/auth/utils'
{=/ isAuthEnabled =}
import type { {= crud.name =} } from "wasp/server/crud";
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

type _WaspEntity = {= crud.entityUpper =}
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
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
const _waspGetAllQuery: {= crud.name =}.GetAllQuery<GetAllInput, GetAllOutput> = ((args, context) => {
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
type GetInput = Prisma.{= crud.entityUpper =}WhereUniqueInput
type GetOutput = _WaspEntity | null
const _waspGetQuery: {= crud.name =}.GetQuery<GetInput, GetOutput> = ((args, context) => {
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
type CreateInput = Prisma.{= crud.entityUpper =}CreateInput
type CreateOutput = _WaspEntity
const _waspCreateAction: {= crud.name =}.CreateAction<CreateInput, CreateOutput> = ((args, context) => {
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
type UpdateInput = Prisma.{= crud.entityUpper =}UpdateInput & Prisma.{= crud.entityUpper =}WhereUniqueInput
type UpdateOutput = _WaspEntity
const _waspUpdateAction: {= crud.name =}.UpdateAction<UpdateInput, UpdateOutput> = ((args, context) => {
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
type DeleteInput = Prisma.{= crud.entityUpper =}WhereUniqueInput
type DeleteOutput = _WaspEntity
const _waspDeleteAction: {= crud.name =}.DeleteAction<DeleteInput, DeleteOutput> = ((args, context) => {
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
    throwInvalidCredentialsError()
  }
  {=/ isAuthEnabled =}
  {=^ isAuthEnabled =}
  // Auth is not enabled
  {=/ isAuthEnabled =}
}