{{={= =}=}}
import type {
  {=# isAuthEnabled =}
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  {=/ isAuthEnabled =}
  {=^ isAuthEnabled =}
  UnauthenticatedActionDefinition,
  UnauthenticatedQueryDefinition,
  {=/ isAuthEnabled =}
  _{= crud.entityUpper =},
} from "../_types";
import type { Prisma } from "@prisma/client";
import type { Payload, SuperJSONObject } from "../../../core/core/serialization/index.js";
import type {
  {= crud.entityUpper =},
} from "../../../core/entities/index.js";
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
export declare namespace {= crud.name =} {
  {=# crud.operations.GetAll =}
  export type GetAllQuery<Input extends Payload = never, Output extends Payload = Payload> = {= queryType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.GetAll =}

  {=# crud.operations.Get =}
  export type GetQuery<Input extends Payload = never, Output extends Payload = Payload> = {= queryType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Get =}

  {=# crud.operations.Create =}
  export type CreateAction<Input extends Payload = never, Output extends Payload = Payload> = {= actionType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Create =}

  {=# crud.operations.Update =}
  export type UpdateAction<Input extends Payload = never, Output extends Payload = Payload> = {= actionType =}<[_WaspEntityTagged], Input, Output>
  {=/ crud.operations.Update =}

  {=# crud.operations.Delete =}
  export type DeleteAction<Input extends Payload = never, Output extends Payload = Payload> = {= actionType =}<[_WaspEntityTagged], Input, Output>
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
type GetInput = SuperJSONObject & Prisma.{= crud.entityUpper =}WhereUniqueInput
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
type CreateInput = SuperJSONObject & Prisma.XOR<
  Prisma.{= crud.entityUpper =}CreateInput,
  Prisma.{= crud.entityUpper =}UncheckedCreateInput
>
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
type UpdateInput = SuperJSONObject & Prisma.XOR<
    Prisma.{= crud.entityUpper =}UpdateInput,
    Prisma.{= crud.entityUpper =}UncheckedUpdateInput
  >
  & Prisma.{= crud.entityUpper =}WhereUniqueInput

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
type DeleteInput = SuperJSONObject & Prisma.{= crud.entityUpper =}WhereUniqueInput
type DeleteOutput = _WaspEntity
export type DeleteActionResolved = {= crud.name =}.DeleteAction<DeleteInput, DeleteOutput>
{=/ overrides.Delete.isDefined =}
{=# overrides.Delete.isDefined =}
const _waspDeleteAction = {= overrides.Delete.importIdentifier =}
export type DeleteActionResolved = typeof _waspDeleteAction
{=/ overrides.Delete.isDefined =}
{=/ crud.operations.Delete =}
