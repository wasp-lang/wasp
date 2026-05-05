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
import type { Payload, SuperJSONObject } from "wasp/core/serialization";
import type {
  {= crud.entityUpper =},
} from "wasp/entities";
import type { CrudOverrideFromRegister } from 'wasp/types'

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
 */
{=# crud.operations.GetAll =}
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
export type RegisteredGetAllQuery = CrudOverrideFromRegister<'{= crud.name =}', 'GetAll', {= crud.name =}.GetAllQuery<GetAllInput, GetAllOutput>>
{=/ crud.operations.GetAll =}

{=# crud.operations.Get =}
type GetInput = SuperJSONObject & Prisma.{= crud.entityUpper =}WhereUniqueInput
type GetOutput = _WaspEntity | null
export type RegisteredGetQuery = CrudOverrideFromRegister<'{= crud.name =}', 'Get', {= crud.name =}.GetQuery<GetInput, GetOutput>>
{=/ crud.operations.Get =}

{=# crud.operations.Create =}
type CreateInput = SuperJSONObject & Prisma.XOR<
  Prisma.{= crud.entityUpper =}CreateInput,
  Prisma.{= crud.entityUpper =}UncheckedCreateInput
>
type CreateOutput = _WaspEntity
export type RegisteredCreateAction = CrudOverrideFromRegister<'{= crud.name =}', 'Create', {= crud.name =}.CreateAction<CreateInput, CreateOutput>>
{=/ crud.operations.Create =}

{=# crud.operations.Update =}
type UpdateInput = SuperJSONObject & Prisma.XOR<
    Prisma.{= crud.entityUpper =}UpdateInput,
    Prisma.{= crud.entityUpper =}UncheckedUpdateInput
  >
  & Prisma.{= crud.entityUpper =}WhereUniqueInput

type UpdateOutput = _WaspEntity
export type RegisteredUpdateAction = CrudOverrideFromRegister<'{= crud.name =}', 'Update', {= crud.name =}.UpdateAction<UpdateInput, UpdateOutput>>
{=/ crud.operations.Update =}

{=# crud.operations.Delete =}
type DeleteInput = SuperJSONObject & Prisma.{= crud.entityUpper =}WhereUniqueInput
type DeleteOutput = _WaspEntity
export type RegisteredDeleteAction = CrudOverrideFromRegister<'{= crud.name =}', 'Delete', {= crud.name =}.DeleteAction<DeleteInput, DeleteOutput>>
{=/ crud.operations.Delete =}
