{{={= =}=}}
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
} from "wasp/server/_types";
import type { Prisma } from "@prisma/client";
import { Payload } from "wasp/server/_types/serialization";
import type {
  {= crud.entityUpper =},
} from "wasp/entities";
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

/**
 * PUBLIC API
 */
export namespace {= crud.name =} {
  {=# crud.operations.GetAll =}
  export type GetAllQuery<Input extends Payload, Output extends Payload> = {= queryType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.GetAll =}

  {=# crud.operations.Get =}
  export type GetQuery<Input extends Payload, Output extends Payload> = {= queryType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Get =}

  {=# crud.operations.Create =}
  export type CreateAction<Input extends Payload, Output extends Payload>= {= actionType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Create =}

  {=# crud.operations.Update =}
  export type UpdateAction<Input extends Payload, Output extends Payload> = {= actionType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Update =}

  {=# crud.operations.Delete =}
  export type DeleteAction<Input extends Payload, Output extends Payload> = {= actionType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Delete =}
}

/**
 * PRIVATE API
 * 
 * The types with the `Resolved` suffix are the types that are used internally by the Wasp client
 * to implement full-stack type safety.
 */
{=# crud.operations.GetAll =}
{=^ overrides.GetAll.isDefined =}
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
export type GetAllQueryResolved = {= crud.name =}.GetAllQuery<GetAllInput, GetAllOutput>
{=/ overrides.GetAll.isDefined =}
{=# overrides.GetAll.isDefined =}
const _waspGetAllQuery = {= overrides.GetAll.importIdentifier =}
export type GetAllQueryResolved = typeof _waspGetAllQuery
{=/ overrides.GetAll.isDefined =}
{=/ crud.operations.GetAll =}

{=# crud.operations.Get =}
{=^ overrides.Get.isDefined =}
type GetInput = Prisma.{= crud.entityUpper =}WhereUniqueInput
type GetOutput = _WaspEntity | null
export type GetQueryResolved = {= crud.name =}.GetQuery<GetInput, GetOutput>
{=/ overrides.Get.isDefined =}
{=# overrides.Get.isDefined =}
const _waspGetQuery = {= overrides.Get.importIdentifier =}
export type GetQueryResolved = typeof _waspGetQuery
{=/ overrides.Get.isDefined =}
{=/ crud.operations.Get =}

{=# crud.operations.Create =}
{=^ overrides.Create.isDefined =}
type CreateInput = Prisma.{= crud.entityUpper =}CreateInput
type CreateOutput = _WaspEntity
export type CreateActionResolved = {= crud.name =}.CreateAction<CreateInput, CreateOutput>
{=/ overrides.Create.isDefined =}
{=# overrides.Create.isDefined =}
const _waspCreateAction = {= overrides.Create.importIdentifier =}
export type CreateActionResolved = typeof _waspCreateAction
{=/ overrides.Create.isDefined =}
{=/ crud.operations.Create =}

{=# crud.operations.Update =}
{=^ overrides.Update.isDefined =}
type UpdateInput = Prisma.{= crud.entityUpper =}UpdateInput & Prisma.{= crud.entityUpper =}WhereUniqueInput
type UpdateOutput = _WaspEntity
export type UpdateActionResolved = {= crud.name =}.UpdateAction<UpdateInput, UpdateOutput>
{=/ overrides.Update.isDefined =}
{=# overrides.Update.isDefined =}
const _waspUpdateAction = {= overrides.Update.importIdentifier =}
export type UpdateActionResolved = typeof _waspUpdateAction
{=/ overrides.Update.isDefined =}
{=/ crud.operations.Update =}

{=# crud.operations.Delete =}
{=^ overrides.Delete.isDefined =}
type DeleteInput = Prisma.{= crud.entityUpper =}WhereUniqueInput
type DeleteOutput = _WaspEntity
export type DeleteActionResolved = {= crud.name =}.DeleteAction<DeleteInput, DeleteOutput>
{=/ overrides.Delete.isDefined =}
{=# overrides.Delete.isDefined =}
const _waspDeleteAction = {= overrides.Delete.importIdentifier =}
export type DeleteActionResolved = typeof _waspDeleteAction
{=/ overrides.Delete.isDefined =}
{=/ crud.operations.Delete =}
