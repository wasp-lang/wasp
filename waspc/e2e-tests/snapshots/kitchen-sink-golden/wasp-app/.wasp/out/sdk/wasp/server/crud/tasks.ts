import type {
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  _Task,
} from "../_types";
import type { Prisma } from "@prisma/client";
import type { Payload, SuperJSONObject } from "wasp/core/serialization";
import type {
  Task,
} from "wasp/entities";
import { getAllTasks as getAllTasks_ext } from 'wasp/src/features/crud/crud'
import { createTask as createTask_ext } from 'wasp/src/features/crud/crud'

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
 *
 * The types with the `Resolved` suffix are the types that are used internally by the Wasp client
 * to implement full-stack type safety.
 */
const _waspGetAllQuery = getAllTasks_ext
export type GetAllQueryResolved = typeof _waspGetAllQuery

type GetInput = SuperJSONObject & Prisma.TaskWhereUniqueInput
type GetOutput = _WaspEntity | null
export type GetQueryResolved = tasks.GetQuery<GetInput, GetOutput>

const _waspCreateAction = createTask_ext
export type CreateActionResolved = typeof _waspCreateAction

type UpdateInput = SuperJSONObject & Prisma.XOR<
    Prisma.TaskUpdateInput,
    Prisma.TaskUncheckedUpdateInput
  >
  & Prisma.TaskWhereUniqueInput

type UpdateOutput = _WaspEntity
export type UpdateActionResolved = tasks.UpdateAction<UpdateInput, UpdateOutput>

type DeleteInput = SuperJSONObject & Prisma.TaskWhereUniqueInput
type DeleteOutput = _WaspEntity
export type DeleteActionResolved = tasks.DeleteAction<DeleteInput, DeleteOutput>
