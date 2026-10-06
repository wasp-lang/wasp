import type {
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  _Task,
} from "../_types";
import type { Prisma } from "@prisma/client";
import type { Payload, SuperJSONObject } from "../../core/serialization/index";
import type {
  Task,
} from "wasp/entities";
import type { FromRegisterPath } from '../../types/register'

type _WaspEntityTagged = _Task
type _WaspEntity = Task

/**
 * PUBLIC API
 */
export declare namespace tasks {
  export type GetAllQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[_WaspEntityTagged], Input, Output>

  export type GetQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[_WaspEntityTagged], Input, Output>

  export type CreateAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[_WaspEntityTagged], Input, Output>

  export type UpdateAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[_WaspEntityTagged], Input, Output>

  export type DeleteAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[_WaspEntityTagged], Input, Output>
}

/**
 * PRIVATE API
 */
type GetAllInput = {}
type GetAllOutput = _WaspEntity[]
export type RegisteredGetAllQuery = FromRegisterPath<['crudOverrides', 'tasks', 'GetAll'], tasks.GetAllQuery<GetAllInput, GetAllOutput>>

type GetInput = SuperJSONObject & Prisma.TaskWhereUniqueInput
type GetOutput = _WaspEntity | null
export type RegisteredGetQuery = FromRegisterPath<['crudOverrides', 'tasks', 'Get'], tasks.GetQuery<GetInput, GetOutput>>

type CreateInput = SuperJSONObject & Prisma.XOR<
  Prisma.TaskCreateInput,
  Prisma.TaskUncheckedCreateInput
>
type CreateOutput = _WaspEntity
export type RegisteredCreateAction = FromRegisterPath<['crudOverrides', 'tasks', 'Create'], tasks.CreateAction<CreateInput, CreateOutput>>

type UpdateInput = SuperJSONObject & Prisma.XOR<
    Prisma.TaskUpdateInput,
    Prisma.TaskUncheckedUpdateInput
  >
  & Prisma.TaskWhereUniqueInput

type UpdateOutput = _WaspEntity
export type RegisteredUpdateAction = FromRegisterPath<['crudOverrides', 'tasks', 'Update'], tasks.UpdateAction<UpdateInput, UpdateOutput>>

type DeleteInput = SuperJSONObject & Prisma.TaskWhereUniqueInput
type DeleteOutput = _WaspEntity
export type RegisteredDeleteAction = FromRegisterPath<['crudOverrides', 'tasks', 'Delete'], tasks.DeleteAction<DeleteInput, DeleteOutput>>
