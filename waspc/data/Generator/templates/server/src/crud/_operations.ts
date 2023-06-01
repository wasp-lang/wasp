{{={= =}=}}
import prisma from "../dbClient.js";

import type {
  {=# isAuthEnabled =}
  AuthenticatedAction,
  AuthenticatedQuery,
  {=/ isAuthEnabled =}
  {=^ isAuthEnabled =}
  Action,
  Query,
  {=/ isAuthEnabled =}
  _{= crud.entityUpper =},
} from "../_types";
import type {
  Prisma,
} from "@prisma/client";
import type {
  {= crud.entityUpper =},
} from "../entities";
{=# isAuthEnabled =}
import { throwInvalidCredentialsError } from "../core/auth.js";
{=/ isAuthEnabled =}
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

type _WaspEntityTagged = _{= crud.entityUpper =}
type _WaspEntity = {= crud.entityUpper =}
const entities = {
  {= crud.entityUpper =}: prisma.{= crud.entityLower =},
}

{=# crud.operations.GetAll =}
{=# isAuthEnabled =}
export type GetAllQuery<Input, Output> = AuthenticatedQuery<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
export type GetAllQuery<Input, Output> = Query<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=# overrides.GetAll.isDefined =}
const _waspGetAllQuery = {= overrides.GetAll.importIdentifier =}
{=/ overrides.GetAll.isDefined =}
{=^ overrides.GetAll.isDefined =}
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
const _waspGetAllQuery: GetAllQuery<GetAllInput, GetAllOutput> = ((args, context) => {
  {=^ crud.operations.GetAll.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.GetAll.isPublic =}
  return context.entities.{= crud.entityUpper =}.findMany();
});
{=/ overrides.GetAll.isDefined =}

export type GetAllQueryResolved = typeof _waspGetAllQuery

export async function getAllFn(args, context) {
  return (_waspGetAllQuery as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.GetAll =}

{=# crud.operations.Get =}
{=# isAuthEnabled =}
export type GetQuery<Input, Output> = AuthenticatedQuery<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
export type GetQuery<Input, Output> = Query<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=# overrides.Get.isDefined =}
const _waspGetQuery = {= overrides.Get.importIdentifier =}
{=/ overrides.Get.isDefined =}
{=^ overrides.Get.isDefined =}
type GetInput = Prisma.{= crud.entityUpper =}WhereUniqueInput
type GetOutput = _WaspEntity | null
const _waspGetQuery: GetQuery<GetInput, GetOutput> = ((args, context) => {
  {=^ crud.operations.Get.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Get.isPublic =}
  return context.entities.{= crud.entityUpper =}.findUnique({ where: { id: args.id } });
});
{=/ overrides.Get.isDefined =}

export type GetQueryResolved = typeof _waspGetQuery

export async function getFn(args, context) {
  return (_waspGetQuery as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Get =}

{=# crud.operations.Create =}
{=# isAuthEnabled =}
export type CreateAction<Input, Output> = AuthenticatedAction<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
export type CreateAction<Input, Output>= Action<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=# overrides.Create.isDefined =}
const _waspCreateAction = {= overrides.Create.importIdentifier =}
{=/ overrides.Create.isDefined =}
{=^ overrides.Create.isDefined =}
type CreateInput = Prisma.{= crud.entityUpper =}CreateInput
type CreateOutput = _WaspEntity
const _waspCreateAction: CreateAction<CreateInput, CreateOutput> = ((args, context) => {
  {=^ crud.operations.Create.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Create.isPublic =}
  return context.entities.{= crud.entityUpper =}.create({ data: args });
});
{=/ overrides.Create.isDefined =}

export type CreateActionResolved = typeof _waspCreateAction

export async function createFn(args, context) {
  return (_waspCreateAction as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Create =}

{=# crud.operations.Update =}
{=# isAuthEnabled =}
export type UpdateAction<Input, Output> = AuthenticatedAction<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
export type UpdateAction<Input, Output> = Action<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=# overrides.Update.isDefined =}
const _waspUpdateAction = {= overrides.Update.importIdentifier =}
{=/ overrides.Update.isDefined =}
{=^ overrides.Update.isDefined =}
type UpdateInput = Prisma.{= crud.entityUpper =}UpdateInput & Prisma.{= crud.entityUpper =}WhereUniqueInput
type UpdateOutput = _WaspEntity
const _waspUpdateAction: UpdateAction<UpdateInput, UpdateOutput> = ((args, context) => {
  {=^ crud.operations.Update.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Update.isPublic =}
  const { {= crud.primaryFieldName =}: primaryFieldValue, ...rest } = args
  return context.entities.{= crud.entityUpper =}.update({
    where: { {= crud.primaryFieldName =}: primaryFieldValue },
    data: rest,
  });
});
{=/ overrides.Update.isDefined =}

export type UpdateActionResolved = typeof _waspUpdateAction

export async function updateFn(args, context) {
  return (_waspUpdateAction as any)(args, {
    ...context,
    entities,
  });
}
{=/ crud.operations.Update =}


{=# crud.operations.Delete =}
{=# isAuthEnabled =}
export type DeleteAction<Input, Output> = AuthenticatedAction<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=^ isAuthEnabled =}
export type DeleteAction<Input, Output> = Action<[_WaspEntityTagged], Input, Output>
{=/ isAuthEnabled =}
{=# overrides.Delete.isDefined =}
const _waspDeleteAction = {= overrides.Delete.importIdentifier =}
{=/ overrides.Delete.isDefined =}
{=^ overrides.Delete.isDefined =}
type DeleteInput = Prisma.{= crud.entityUpper =}WhereUniqueInput
type DeleteOutput = _WaspEntity
const _waspDeleteAction: DeleteAction<DeleteInput, DeleteOutput> = ((args, context) => {
  {=^ crud.operations.Delete.isPublic =}
  throwIfNotAuthenticated(context)
  {=/ crud.operations.Delete.isPublic =}
  return context.entities.{= crud.entityUpper =}.delete({ where: args });
});
{=/ overrides.Delete.isDefined =}

export type DeleteActionResolved = typeof _waspDeleteAction

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
