import type {
  AuthenticatedActionDefinition,
  AuthenticatedQueryDefinition,
  _Task,
} from "../_types";
import type { Prisma } from "@prisma/client";
import type { Payload, SuperJSONObject } from "../../core/serialization/index.js";
import type {
  Task,
} from "../../entities/index.js";
import { getAllModuleTodos as getAllModuleTodos_ext } from '@kitchen-sink/module/crud'

type _WaspEntityTagged = _Task
type _WaspEntity = Task

/**
 * PUBLIC API
 */
export declare namespace moduleTodos {
  export type GetAllQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[_WaspEntityTagged], Input, Output>



  export type UpdateAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[_WaspEntityTagged], Input, Output>

  export type DeleteAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[_WaspEntityTagged], Input, Output>
}

/**
 * PRIVATE API
 *
 * The types with the `Resolved` suffix are the types that are used internally by the Wasp client
 * to implement full-stack type safety.
 */
const _waspGetAllQuery = getAllModuleTodos_ext
export type GetAllQueryResolved = typeof _waspGetAllQuery



type UpdateInput = SuperJSONObject & Prisma.XOR<
    Prisma.TaskUpdateInput,
    Prisma.TaskUncheckedUpdateInput
  >
  & Prisma.TaskWhereUniqueInput

type UpdateOutput = _WaspEntity
export type UpdateActionResolved = moduleTodos.UpdateAction<UpdateInput, UpdateOutput>

type DeleteInput = SuperJSONObject & Prisma.TaskWhereUniqueInput
type DeleteOutput = _WaspEntity
export type DeleteActionResolved = moduleTodos.DeleteAction<DeleteInput, DeleteOutput>
